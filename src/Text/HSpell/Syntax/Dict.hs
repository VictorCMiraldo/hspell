{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Defines the syntactical dictionary responsible for
-- performing spelling checks and suggestions.
--
-- TODO: Add Part-Of-Speech tags here too!
module Text.HSpell.Syntax.Dict where

import Prelude hiding (lookup)

import           Data.Text (Text)
import           Data.Default
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T (readFile)
import qualified Data.Text.Metrics  as T (damerauLevenshtein)

import           Data.Maybe (mapMaybe)
import           Data.List (foldl')
import           Data.Word (Word32)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set           as S

import Control.Exception
import Control.Concurrent

import Text.HSpell.Util

-- |Dictionary configuration. 
data DictConfig = DictConfig
  { dcMaxDistance  :: Int
  , dcPrefixLength :: Int
  } deriving (Eq , Show)

instance Default DictConfig where
  def = DictConfig 2 5

-- |A Dictionary is a @bytestring-trie@ with `DictEntry`.
--
-- TODO: Experiment with hashtables!
-- TODO: Experiment with compact package
data Dict = Dict
  { dConf    :: ! DictConfig                       -- ^ Stores config parameters.
  , dCorrect :: ! (HM.HashMap Word32 (S.Set Text)) -- ^ Stores the actual dictionary
  , dDeletes :: ! (HM.HashMap Word32 (S.Set Text)) -- ^ Stores deletions
  } 

combineDicts :: DictConfig -> [Dict] -> Dict
combineDicts dc []       = empty dc
combineDicts dc ds@(_:_) =
  let cors = map dCorrect ds
      dels = map dDeletes ds
   in Dict dc (go cors) (go dels)
 where
   go :: [HM.HashMap Word32 (S.Set Text)] -> HM.HashMap Word32 (S.Set Text)
   go []     = HM.empty
   go [x]    = x
   go (x:xs) = HM.unionWith S.union x (go xs)


dictOnCorrect :: (HM.HashMap Word32 (S.Set Text) -> HM.HashMap Word32 (S.Set Text))
              -> Dict -> Dict
dictOnCorrect f d = d { dCorrect = f (dCorrect d) }

dictOnDeletes :: (HM.HashMap Word32 (S.Set Text) -> HM.HashMap Word32 (S.Set Text))
              -> Dict -> Dict
dictOnDeletes f d = d { dDeletes = f (dDeletes d) }

-- |Empy dictionary
empty :: DictConfig -> Dict
empty dc = Dict dc HM.empty HM.empty

-- |Inserts an correct entry into the dictionary; 
insertCorrect :: Text -> Dict -> Dict
insertCorrect t = dictOnDeletes (HM.insertWith S.union (djb2 t) (S.singleton t))

insertDeletes :: Text -> Text -> Dict -> Dict
insertDeletes t dt = dictOnDeletes (HM.insertWith S.union (djb2 dt) (S.singleton t))

-- |Looking up whether a word belongs in the dictionary /does not/
-- return spelling suggestions, use 'spellcheck' for that purpose.
lookupCorrect :: Text -> Dict -> Bool
lookupCorrect t = maybe False (S.member t) . HM.lookup (djb2 t) . dCorrect

lookupSuggestions :: Text -> Dict -> Maybe (S.Set Text)
lookupSuggestions t = HM.lookup (djb2 t) . dDeletes

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
spellcheck' t d 
  | lookupCorrect t d = Nothing
  | otherwise = let ts  = candidates d t
                 in Just $ S.unions $ mapMaybe (flip lookupSuggestions d) (t:ts) 

-- |Refines the suggestions in 'DictEntry' for a specific query.
refineFor :: DictConfig -> Text -> S.Set Text -> S.Set Text
refineFor dc t = S.filter (\ u -> T.damerauLevenshtein t u <= dcMaxDistance dc)

-- |Spell-checks and refines the suggestions.
spellcheck :: Text -> Dict -> Maybe (S.Set Text)
spellcheck t d = refineFor (dConf d) t <$> spellcheck' t d

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
    -- for now, returns just the word in a line separated by ';'.
    -- later, will return POS too
    parseLine :: Text -> Either ParseError Text
    parseLine t = case T.splitOn ";" t of
                    []    -> Left ("can't parse: " ++ show t)
                    (x:_) -> Right x
    
    go :: Text -> Dict -> Either ParseError Dict
    go t d = do tl <- parseLine t
                let es = candidates d tl
                return $ foldl' (\d' dt -> insertDeletes tl dt d') (insertCorrect tl d) es

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

largeDict1 :: IO Dict
largeDict1 = do ed <- loadDict def "dict/en-80k.txt"
                case ed of
                  Left err -> error err
                  Right d  -> return d


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

{-
lkup :: Dict -> Text -> [DictEntry]
lkup d t = let ts = textDeletes t
            in mapMaybe (flip Tr.lookup d . T.encodeUtf8) (t:ts)
-}



test :: [Text]
test = T.chunksOf 1 "abcde"

