module Text.HSpell.Base.Parser where

import Prelude hiding (readFile)

import Data.Char
import Data.Text (pack)
import Control.Monad

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Pos
--------------------
import Text.HSpell.Base.Types

-- |Parses a stream of 'Text'.
type HSpellParser = Parsec Text ()

-- |Annotates the result of a parser with source location.
loc :: HSpellParser Loc
loc = do
  pos <- getPosition 
  return (Loc (sourceLine pos - 1) (sourceColumn pos - 1))

-- |Removes whitespaces after the given parser but
-- return the location where the spaces begin being skipped.
lexeme' :: HSpellParser a -> HSpellParser (a , Loc)
lexeme' p = ((,) <$> p <*> loc) <* skipMany space

-- |Removes whitespaces after the given parser
lexeme :: HSpellParser a -> HSpellParser a
lexeme p = p <* skipMany space

-- |Parses a sequence of letters, returns the position
-- where said sequence /ends/.
word :: HSpellParser (Text , Loc)
word = lexeme' (pack <$> many1 letter)

-- |Parses punctiation signs (does not include full stops!).
-- Like 'word', returns the location where the sequence finishes.
punct :: HSpellParser (Text , Loc)
punct = lexeme' (pack <$> many1 (oneOf punctuation))
  where
    punctuation = ",!?/\\|;:[]{}()@#$%^&*-_"

-- |Given a parser that returns the location of the last parsed
-- character, returns a parser that additionally returs the
-- location of the first parsec character.
section :: HSpellParser (a , Loc) -> HSpellParser (a , Section)
section p = do
  start       <- loc
  (res , end) <- p
  return (res , (start , end))

-- |Parses a word in the form of a 'Token'
tokenWord :: HSpellParser Token
tokenWord = uncurry Token <$> section word
  
-- |Parses some punctuation sign with its source location
tokenPunct :: HSpellParser Token
tokenPunct = uncurry Token <$> section punct

-- |Parses the specific token
hstoken :: String -> HSpellParser ()
hstoken s = try $ do
  ss <- lexeme (many1 (satisfy (not . isSpace)))
  when (s /= ss) (fail "" <?> s)
  
