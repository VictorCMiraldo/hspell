{-# LANGUAGE OverloadedStrings #-}
module Text.HSpell.Grammar.Rule where

import qualified Data.Text as T

import           Data.Maybe (mapMaybe)
import qualified Data.Set  as S
import           Data.Aeson
import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import           Text.Parsec

import           Control.Arrow ((***))
import           Control.Monad.Reader
--------------------------------------
import Text.HSpell.Base
import Text.HSpell.Grammar.Matcher

-- |Encodes a grammatical suggestion; examples include:
--
-- > - match: in order to
-- >   suggest: to
-- >   descr: Using 'in order to' is pleonasm.
-- >
-- > - match: good in \1@\noum # Part-of-speech tags are not yet supported
-- >   suggest: good at \1
-- >   descr: If you mean skilled in \1, use "at".
--
-- > - match: \1@\. \1
-- >   suggest: \1
-- >   descr: \1 is repeated, are you sure it should be this way?
-- >   
--
data GrammarRule = GrammarRule
  { grMatch       :: Rule Int T.Text
  , grSuggestion  :: T.Text
  , grDescription :: T.Text
  } deriving Show

instance FromJSON GrammarRule where
    parseJSON = withObject "GrammarRule" $ \c -> do
      rtxt <- c .: "match"
      sug  <- c .: "suggest"
      desc <- c .: "descr"
      case parseRule rtxt of
        Left err -> fail err
        Right r  -> return $ GrammarRule r sug desc

-- |Describes a set of grammar rules. For now it's just a list but I
-- can see some sort of trie structure used to merge rules if (when? haha)
-- efficiency issues arise.
type GrammarRuleSet = [GrammarRule]

grammarcheck :: GrammarRuleSet -> Sentence -> [Suggest]
grammarcheck rs s = mapMaybe (flip grammarcheck1 s) rs

-- TODO: what about rules that can match multiple times, like duplicate
-- words?

-- |Attemps to match a grammatical rule to a sentence; if it matches,
-- we produce a suggestion out of it.
grammarcheck1 :: GrammarRule -> Sentence -> Maybe Suggest
grammarcheck1 r s = do
  (i , cnt , vs) <- match (grMatch r) (map tText s)
  return $ Suggest (getSection i cnt s)
                   (S.singleton $ textFormat (map (T.pack . show *** id) vs)
                                             (grSuggestion r))
 where
   -- TODO: test these monsters; this can break spetacularly! :)
   getSection :: Int -> Int -> Sentence -> Section
   getSection i cnt = sectionFor . take cnt . drop i

   sectionFor :: [Token] -> Section
   sectionFor ts = (fst $ tSect (head ts) , snd $ tSect (last ts))
   

-- |Replaces escaped occureences of text by some specified value.
--
-- > textFormat [("1" , "first") , ("2" , "second")] "today is the \\1 day"
-- >  == "today is the first day"
textFormat :: [(T.Text , T.Text)] -> T.Text -> T.Text
textFormat db t0 = T.concat . map replace $ T.splitOn "\\" t0
  where
    replace t = let (bf, af) = T.breakOn " " t
                 in case lookup bf db of
                      Just res -> T.concat [res , af]
                      Nothing  -> t

-- * Parsing and Loading

-- |Parses a rule using int for variable names and tokens being any word.
parseRule :: T.Text -> Either String (Rule Int T.Text)
parseRule = either (Left . show) Right . parse parseSimpleRule ""

loadRulesFromFile :: FilePath -> IO (Either Y.ParseException GrammarRuleSet)
loadRulesFromFile path = Y.decodeEither' <$> BS.readFile path
