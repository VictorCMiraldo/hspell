module Text.HSpell.Syntax.Dict.Parser (loadDictFromFile) where

import Prelude hiding (readFile)
import System.Exit

import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO (readFile)

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Combinator
import Text.Parsec.Char
--------------------
import Text.HSpell.Base.Parser
import Text.HSpell.Syntax.Dict


{-
-- TODO: Add part-of-speech mechanisms; this is all just a huge
-- sketch

parseOf :: HSpellParser Text
parseOf = token "of" >> word

parsePOS' :: HSpellParser PartOfSpeech
parsePOS' = choice
  [ token "npl"  *> (NoumPlural <$> optionMaybe parseOf)
  , token "n"    *> return Noum
  , token "a"    *> return Adjective
  , token "adv"  *> return Adverb
  , token "pron" *> return Pronoum
  , token "prep" *> return Preposition
  , token "pl"   *> (Plural <$> optionMaybe parseOf)
  , token "conj" *> return Conjunction
  , parseVerb
  ]

parseVerb :: HSpellParser PartOfSpeech
parseVerb = do
  vf  <- parseVerbForm `sepBy` (token "&")
  of_ <- optionMaybe parseOf
  return (Verb vf of_)


parseVerbForm :: HSpellParser VerbForm
parseVerbForm = choice
  [ token "pp"  *> return PastParticiple
  , token "imp" *> return PastSimple
  , token "ppr" *> return PastParticiple
  , token "vt"  *> return Transitive
  , token "vi"  *> return Intransitive
  ]

-- |Parses a list of /part-of-speech/ tokens
parsePOS :: [Text] -> Either String [PartOfSpeech]
parsePOS = either (Left . show) Right . sequence
         . map (parse parsePOS' "")
-}

-- |Parses a line such as @was;2751347404;imp of be;v@
parseEntry :: HSpellParser (Text , DictEntry)
parseEntry = do
  w   <- word' <* lexeme (char ';')
  f   <- read <$> many1 digit -- <* lexeme (char ';')
  -- pos <- parsePOS' `sepBy` (lexeme (char ';'))
  skipMany (noneOf "\n")
  return (w , DictEntry f)

parseDict :: HSpellParser [(Text , DictEntry)] 
parseDict = parseEntry `sepEndBy` newline

buildDict :: DictConfig -> [(Text , DictEntry)] -> Dict
buildDict dc = foldl' go (empty dc)
  where
    go :: Dict -> (Text , DictEntry) -> Dict
    go d (t, de) = let es = candidates d t
                    in foldl' (\d' dt -> insertDeletes t dt d')
                              (insertCorrect t de d) es

--

loadDictFromFile :: DictConfig -> FilePath -> IO Dict
loadDictFromFile dc file = do
  cont <- readFile file
  let res = parse (parseDict <* eof) file cont
  case res of
    Left err      -> putStrLn (show err) >> exitFailure
    Right entries -> return $ buildDict dc entries



