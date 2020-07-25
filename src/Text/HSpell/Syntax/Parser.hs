
module Text.HSpell.Syntax.Parser (loadDictFromFile) where

import Prelude hiding (readFile)
import System.Exit

import Data.Char
import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import Control.Monad

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Combinator
import Text.Parsec.Char
--------------------
import Text.HSpell.Syntax.Dict

type Parser = Parsec Text ()

lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany space

word :: Parser Text
word = lexeme (pack <$> many1 letter)

token :: String -> Parser ()
token s = try $ do
  ss <- lexeme (many1 (satisfy (not . isSpace)))
  when (s /= ss) (fail "" <?> s)
  
{-
-- TODO: Add part-of-speech mechanisms; this is all just a huge
-- sketch

parseOf :: Parser Text
parseOf = token "of" >> word

parsePOS' :: Parser PartOfSpeech
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

parseVerb :: Parser PartOfSpeech
parseVerb = do
  vf  <- parseVerbForm `sepBy` (token "&")
  of_ <- optionMaybe parseOf
  return (Verb vf of_)


parseVerbForm :: Parser VerbForm
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
parseEntry :: Parser (Text , DictEntry)
parseEntry = do
  w   <- word                 <* lexeme (char ';')
  f   <- read <$> many1 digit -- <* lexeme (char ';')
  -- pos <- parsePOS' `sepBy` (lexeme (char ';'))
  skipMany (noneOf "\n")
  return (w , DictEntry f)

parseDict :: Parser [(Text , DictEntry)] 
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



