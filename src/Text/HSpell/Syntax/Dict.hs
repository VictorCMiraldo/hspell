{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Defines the syntactical dictionary responsible for
-- performing spelling checks and suggestions. We use the @SymSpell@
-- algorithm under the hood to perform suggestions.
--
-- TODO: Add Part-Of-Speech tags here too!
module Text.HSpell.Syntax.Dict where

import Prelude hiding (lookup)

import           Control.Arrow ((&&&))

import           Data.Function (on)
import           Data.List (sortBy, groupBy)
import           Data.Text (Text)
import           Data.Default
import           Data.Maybe (fromJust)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Metrics  as T (damerauLevenshtein)

import           Data.Maybe (mapMaybe)
import qualified Data.Trie             as Tr
import qualified Data.Trie.Convenience as Tr
import qualified Data.Set              as S

import Text.HSpell.Util

data DictEntry = DictEntry
  { deFreq :: Int
  } deriving (Eq , Show)

-- |Dictionary configuration.
data DictConfig = DictConfig
  { dcMaxDistance  :: Int -- ^ Indicates what is the maximum Damerau-Leveshtein distance to be considered
                          -- when looking for suggestions. 
  , dcPrefixLength :: Int -- ^ Looks only at the first 'dcPrefixLength' letters of the input
                          -- when precomputing and spellchecking.
  } deriving (Eq , Show)

instance Default DictConfig where
  def = DictConfig 2 4

-- |A Dictionary is a @bytestring-trie@ with `DictEntry`.
--
-- TODO: Experiment with compact package
-- TODO: Experiment with different output verbosity: /All/,/Top/,/Some/
data Dict = Dict
  { dConf    :: ! DictConfig             -- ^ Stores config parameters.
  , dCorrect :: ! (Tr.Trie DictEntry)    -- ^ Stores the actual dictionary
  , dDeletes :: ! (Tr.Trie (S.Set Text)) -- ^ Stores deletions
  } 

-- |Combines two dictionaries into one; calls @error@ on overlapping correct entries.
--
-- TODO: should it really call error? I don't think so.
combineDicts :: DictConfig -> [Dict] -> Dict
combineDicts dc []       = empty dc
combineDicts dc [d]      = d
combineDicts dc ds@(_:_) =
  let cors = map dCorrect ds
      dels = map dDeletes ds
   in Dict dc (go (error "Overlapping entries") cors) (go S.union dels)
 where
   go :: (a -> a -> a) -> [Tr.Trie a] -> Tr.Trie a
   go _ []     = Tr.empty
   go _ [x]    = x
   go f (x:xs) = Tr.mergeBy (\a b -> Just $! f a b) x (go f xs)


dictOnCorrect :: (Tr.Trie DictEntry -> Tr.Trie DictEntry)
              -> Dict -> Dict
dictOnCorrect f d = d { dCorrect = f (dCorrect d) }

dictOnDeletes :: (Tr.Trie (S.Set Text) -> Tr.Trie (S.Set Text))
              -> Dict -> Dict
dictOnDeletes f d = d { dDeletes = f (dDeletes d) }

-- |Empy dictionary
empty :: DictConfig -> Dict
empty dc = Dict dc Tr.empty Tr.empty

-- |Inserts an correct entry into the dictionary; 
insertCorrect :: Text -> DictEntry -> Dict -> Dict
insertCorrect t = dictOnCorrect . Tr.insert (T.encodeUtf8 t)

-- |Inserts a precomputed deletion of an entry into a dictionary
insertDeletes :: Text -> Text -> Dict -> Dict
insertDeletes t dt = dictOnDeletes (Tr.insertWith S.union (T.encodeUtf8 dt) (S.singleton t))

-- |Looking up whether a word belongs in the dictionary /does not/
-- return spelling suggestions, use 'spellcheck' for that purpose.
lookupCorrect :: Text -> Dict -> Maybe DictEntry
lookupCorrect t = Tr.lookup (T.encodeUtf8 t) . dCorrect

-- |Looks up whether we have suggestions for a given word.
lookupSuggestions :: Text -> Dict -> Maybe (S.Set Text)
lookupSuggestions t = Tr.lookup (T.encodeUtf8 t) . dDeletes

-- |Generates candidates for a given text.
candidates :: Dict -> Text -> [Text]
candidates d = textDeletesN (dcMaxDistance $ dConf d)
             . T.take (dcPrefixLength $ dConf d)

-- |Spell-checks a given word. The idea is that we will be performing a
-- number of lookups. For example, say we are spell-checking the word @yse@.
--
-- We would lookup: @["yse", "ys" , "ye" , "se"]@ on the dictionary. Note that
-- the dictonary contains @"yes"@, hence, it also contains the strings @["ye", "ys" , "es"]@,
-- which are deletions from @"yes"@.
--
-- The 'spellcheck'' function /does not/ refine the set of suggestions in 'DictEntry'.
-- Use 'spellcheck' or 'refineFor' for that.
spellcheck' :: Text -> Dict -> Maybe (S.Set Text)
spellcheck' t d =
  case lookupCorrect t d of
   Just _  -> Nothing
   Nothing -> let ts  = candidates d t
               in Just $ S.unions $ mapMaybe (flip lookupSuggestions d) (t:ts) 

-- |Refines the suggestions in 'DictEntry' for a specific query, namelly,
-- returns only the suggestions within 'dcMaxDistance' from the queried word
-- and adds frequency information.
refineFor :: Dict -> Text -> S.Set Text -> S.Set (Text , (Int , Int))
refineFor d t = S.filter (\(_ , (dist , _)) -> dist <= dcMaxDistance (dConf d))
              . S.map (id &&& T.damerauLevenshtein t
                          &&& deFreq . fromJust . flip lookupCorrect d) 

-- |How to provide output.
data SymSpellVerbosity
  = All      -- ^ Returns all possible candidates
  | Closest  -- ^ Returns all candidates with smallest distance from the input
  | Top      -- ^ Returns the closest and most frequent candidate.
  deriving (Eq , Show)

-- |Spell-checks a given word returning /all/ possible suggestions
-- in case it detects a misspell. A result of 'Nothing' indicates that
-- the given word is /correct/. 
spellcheckWordAll :: Text -> Dict -> Maybe (S.Set (Text, (Int , Int)))
spellcheckWordAll t d = refineFor d t <$> spellcheck' t d

-- |Spell checks a word with a given 'SymSpellVerbosity' verbosity option.
spellcheckWord :: SymSpellVerbosity -> Text -> Dict -> Maybe (S.Set Text)
spellcheckWord All  t d = fmap (S.map fst) $ spellcheckWordAll t d
spellcheckWord verb t d = do
  sugs <- S.toList <$> spellcheckWordAll t d
  let ssug = groupBy ((==) `on` (fst . snd)) $ sortBy (compare `on` snd) sugs
  return $ case ssug of
    []    -> S.empty
    (c:_) -> S.fromList $ case verb of
               Top     -> [fst (head c)]
               Closest -> map fst c
               _       -> error "unreachable"
  

{-
------------------------------
-- * Loading a Dictionary * --
------------------------------

type ParseError = String

-- |Loads a dictionary from a file
--
-- TODO: Load times are slow; maybe do some parallel magic then
--       merge the tries?
loadDict :: DictConfig -> FilePath -> IO (Either ParseError Dict)
loadDict dc f = buildDict dc <$> T.readFile f 

buildDict :: DictConfig -> Text -> Either ParseError Dict
buildDict dc = foldl' (\pe t -> pe >>= go t) (return $ empty dc) . T.lines
  where
    go :: Text -> Dict -> Either ParseError Dict
    go t d = do (tl , f , pos) <- parseLine t
                let de = DictEntry pos f
                let es = candidates d tl
                return $ foldl' (\d' dt -> insertDeletes tl dt d')
                                (insertCorrect tl de d) es

-------------------
-------------------

-- Concurrent dict loading?

type Children t = MVar [MVar (Either SomeException t)]

waitForChildren :: Children t -> IO (Either SomeException [t])
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    []   -> return (Right [])
    m:ms -> do
       putMVar children ms
       x  <- takeMVar m
       xs <- waitForChildren children
       return $ (:) <$> x <*> xs

forkChild :: Children t -> IO t -> IO ThreadId
forkChild children io = do
    mvar   <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    tid <- forkFinally io (\res -> putMVar mvar res)
    putStrLn ("Forked: " ++ show tid)
    return tid

loadDictL :: DictConfig -> [FilePath] -> IO (Either ParseError Dict)
loadDictL dc fs = do
  children <- newMVar []
  ds       <- either throw sequence
              <$> mapConcIO children (loadDict dc) fs
  return (either (Left . id) (Right . combineDicts dc) ds) 
 where
    mapConcIO :: Children t -> (a -> IO t) -> [a] -> IO (Either SomeException [t])
    mapConcIO children f as = do
      _ <- mapM (forkChild children . f) as
      waitForChildren children


-------------------
-------------------

largeDict :: IO Dict
largeDict = do ed <- loadDict def "dict/en-with-freq.wl"
               case ed of
                 Left err -> error err
                 Right d  -> return d

{-

largeDict :: IO Dict
largeDict = do ed <- loadDictL def dictFiles
               case ed of
                 Left err -> error err
                 Right d  -> return d
   where
     dictFiles = [ "dict/en/" ++ c:".wl" | c <- ['a' .. 'z'] ]

smallDict :: Dict
smallDict = case buildDict def t of
              Left err -> error err
              Right d  -> d
 where
   t = T.unlines [ "yes;" , "maybe;" , "no;"
                 , "an;" , "tomorrow;" , "bank;"
                 , "banked;" , "disband;" ]

lkup :: Dict -> Text -> [DictEntry]
lkup d t = let ts = textDeletes t
            in mapMaybe (flip Tr.lookup d . T.encodeUtf8) (t:ts)
-}



test :: [Text]
test = T.chunksOf 1 "abcde"

-}
