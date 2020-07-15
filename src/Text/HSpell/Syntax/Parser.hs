
module Text.HSpell.Syntax.Parser where


import Data.Char
import Data.List (foldl')
import Data.Text (Text, pack)
import Control.Monad

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Combinator
import Text.Parsec.Char
--------------------
import Text.HSpell.Syntax.POS
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

-- |Parses a line such as @was;2751347404;imp of be;v@
parseEntry :: Parser (Text , DictEntry)
parseEntry = do
  w   <- word                 <* lexeme (char ';')
  f   <- read <$> many1 digit <* lexeme (char ';')
  pos <- parsePOS' `sepBy` (lexeme (char ';'))
  return (w , DictEntry pos f)

parseDict :: Parser [(Text , DictEntry)] 
parseDict = parseEntry `sepBy` newline

buildDict :: DictConfig -> [(Text , DictEntry)] -> Dict
buildDict dc = foldl' go (empty dc)
  where
    go :: Dict -> (Text , DictEntry) -> Dict
    go d (t, de) = let es = candidates d t
                    in foldl' (\d' dt -> insertDeletes t dt d')
                              (insertCorrect t de d) es

test = parse parseDict "" $ T.lines
  ["was;123;a"
  ,"be;432;n"
  ]
