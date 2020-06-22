{-# LANGUAGE OverloadedStrings #-}
module Text.HSpell.Syntax where

import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T (readFile)

import           Data.Maybe (mapMaybe)
import           Data.List (foldl')
import qualified Data.Trie             as Tr
import qualified Data.Trie.Convenience as Tr
import qualified Data.Set  as S


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
  
newtype Dict = Dict (Tr.Trie DictEntry)

emptyDict :: Dict
emptyDict = Dict Tr.empty

type ParseError = String

-- |Loads a dictionary from a file
--
-- TODO: Load times are slow; maybe do some parallel magic then
--       merge the tries?
loadDict :: FilePath -> IO (Either ParseError Dict)
loadDict f = buildDict <$> T.readFile f 

buildDict :: Text -> Either ParseError Dict
buildDict = foldl' (\pe t -> pe >>= go t) (return emptyDict)
          . T.lines
  where
    -- for now, returns just the word in a line separated by ';'.
    -- later, will return POS too
    parseLine :: Text -> Either ParseError Text
    parseLine t = case T.breakOnAll ";" t of
                    []    -> Left ("can't parse: " ++ show t)
                    (x:_) -> Right (fst x)
    
    ins :: Dict -> (Text , DictEntry) -> Dict
    ins d (t , e) = Tr.insertWith (<>) (T.encodeUtf8 t) e d
    

    go :: Text -> Dict -> Either ParseError Dict
    go t d = do tl <- parseLine t
                let es = genEntriesFor tl
                return $ foldl' ins d es

-- |@deletes xs@ generates a list of lists that correspond to one deletion
-- from @xs@.
deletes :: [a] -> [[a]]
deletes []     = []
deletes (x:xs) = xs : map (x:) (deletes xs)

-- |Generates all deletions for a given word
textDeletes :: Text -> [Text]
textDeletes = map T.concat . deletes . T.chunksOf 1 

genEntriesFor :: Text -> [(Text , DictEntry)]
genEntriesFor t = let ts   = textDeletes t 
                      forT = S.singleton t
                   in (t , DictEntry True S.empty)
                    : map (\t' -> (t' , DictEntry False forT)) ts


largeDict :: IO Dict
largeDict = do ed <- loadDict "dict.wl"
               case ed of
                 Left err -> error err
                 Right d  -> return d

smallDict :: Dict
smallDict = case buildDict t of
              Left err -> error err
              Right d  -> d
 where
   t = T.unlines [ "yes;" , "maybe;" , "no;"
                 , "an;" , "tomorrow;" , "bank;"
                 , "banked;" , "disband;" ]

lkup :: Dict -> Text -> [DictEntry]
lkup d t = let ts = textDeletes t
            in mapMaybe (flip Tr.lookup d . T.encodeUtf8) (t:ts)



test :: [Text]
test = T.chunksOf 1 "abcde"

