{-# LANGUAGE OverloadedStrings #-}
module Text.HSpell.Grammar.Rule where

import qualified Data.Text as T

import           Data.Aeson
import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import           Text.Parsec
--------------------------------------
import Text.HSpell.Base
import Text.HSpell.Grammar.Matcher

-- |Encodes a grammatical suggestion; examples include:
--
-- > - match: in order to
-- >   suggest: to
-- >   descr: Using 'in order to' is pleonasm.
-- >
-- > - match: good in \1@\noum
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

-- |Parses a rule using int for variable names and tokens being
-- any word.
parseRule :: T.Text -> Either String (Rule Int T.Text)
parseRule = either (Left . show) Right . parse parseSimpleRule ""

instance FromJSON GrammarRule where
    parseJSON = withObject "GrammarRule" $ \c -> do
      rtxt <- c .: "match"
      sug  <- c .: "suggest"
      desc <- c .: "descr"
      case parseRule rtxt of
        Left err -> fail err
        Right r  -> return $ GrammarRule r sug desc

loadRulesFromFile :: FilePath -> IO (Either Y.ParseException [GrammarRule])
loadRulesFromFile path = Y.decodeEither' <$> BS.readFile path
