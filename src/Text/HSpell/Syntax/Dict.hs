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
import qualified Data.Trie             as Tr
import qualified Data.Trie.Convenience as Tr
import qualified Data.Set  as S

import Text.HSpell.Util

-- |A dictionary entry consists in a flag about whether or not the key is a
-- correct spelled word and a list of possible suggestions.
data DictEntry = DictEntry
  { isCorrect  :: Bool       -- ^ Does this entry correspond to a correct word?
  , misspellOf :: S.Set Text -- ^ This key could a mispel of any of the words
                             -- in `misspellOf`.
  -- TODO: add part-of-speech
  } deriving Show

instance Semigroup DictEntry where
  de <> dd = DictEntry (isCorrect de || isCorrect dd)
                       (misspellOf de `S.union` misspellOf dd)

instance Monoid DictEntry where
  mempty = DictEntry False S.empty

-- |Dictionary configuration. 
data DictConfig = DictConfig
  { dcMaxDistance :: Int
  } deriving (Eq , Show)

instance Default DictConfig where
  def = DictConfig 2

-- |A Dictionary is a @bytestring-trie@ with `DictEntry`.
--
-- TODO: Experiment with hashtables!
-- TODO: Experiment with compact package
data Dict = Dict
  { dConf :: ! DictConfig          -- ^ Stores config parameters.
  , dTrie :: ! (Tr.Trie DictEntry) -- ^ Stores the actual dictionary
  }

dictOnTrie :: (Tr.Trie DictEntry -> Tr.Trie DictEntry) -> Dict -> Dict
dictOnTrie f d = d { dTrie = f (dTrie d) }

-- |Empy dictionary
empty :: DictConfig -> Dict
empty dc = Dict dc Tr.empty

-- |Inserts an entry into the dictionary; uses the @(Semigroup.<>)@ operation
-- in case of conflicts.
insert :: Text -> DictEntry -> Dict -> Dict
insert t e = dictOnTrie (Tr.insertWith (<>) (T.encodeUtf8 t) e)
    
-- |Looking up whether a word belongs in the dictionary /does not/
-- return spelling suggestions, use 'spellcheck' for that purpose.
lookup :: Text -> Dict -> Maybe DictEntry
lookup t = Tr.lookup (T.encodeUtf8 t) . dTrie

-- |Generates candidates for a given text.
candidates :: Dict -> Text -> [Text]
candidates d = textDeletesN (dcMaxDistance $ dConf d)

-- |Spell-checks a given word. The idea is that we will be performing a
-- number of lookups. For example, say we are spell-checking the word @yse@.
--
-- We would lookup: @["yse", "ys" , "ye" , "se"]@ on the dictionary. Note that
-- the dictonary contains @"yes"@, hence, it also contains the strings @["ye", "ys" , "es"]@,
-- which are deletions from @"yes"@.
--
-- The 'spellcheck'' function /does not/ refine the set of suggestions in 'DictEntry'.
-- Use 'spellcheck' or 'refineFor' for that.
spellcheck' :: Text -> Dict -> DictEntry
spellcheck' t d =
  let ts  = candidates d t
   in mconcat $ mapMaybe (flip lookup d) (t:ts) 

-- |Refines the suggestions in 'DictEntry' for a specific query.
refineFor :: DictConfig -> Text -> DictEntry -> DictEntry
refineFor dc t e = e { misspellOf = aux (misspellOf e) }
  where
    aux :: S.Set Text -> S.Set Text
    aux = S.filter (\ u -> T.damerauLevenshtein t u <= dcMaxDistance dc)

-- |Spell-checks and refines the suggestions.
spellcheck :: Text -> Dict -> DictEntry
spellcheck t d = refineFor (dConf d) t $ spellcheck' t d

size :: Dict -> Int
size = Tr.size . dTrie

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
    parseLine t = case T.breakOnAll ";" t of
                    []    -> Left ("can't parse: " ++ show t)
                    (x:_) -> Right (fst x)
    
    go :: Text -> Dict -> Either ParseError Dict
    go t d = do tl <- parseLine t
                let es = genEntriesFor d tl
                return $ foldl' (flip (uncurry insert)) d es

genEntriesFor :: Dict -> Text -> [(Text , DictEntry)]
genEntriesFor d t = let ts   = candidates d t 
                        forT = S.singleton t
                     in (t , DictEntry True S.empty)
                      : map (\t' -> (t' , DictEntry False forT)) ts


-------------------
-------------------

largeDict :: IO Dict
largeDict = do ed <- loadDict def "dict.wl"
               case ed of
                 Left err -> error err
                 Right d  -> return d

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

