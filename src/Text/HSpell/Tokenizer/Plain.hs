-- |Provides a simple tokenizer for plain text.
-- Sentences are split on full stops ('.').
module Text.HSpell.Tokenizer.Plain where

import Text.Parsec
-------------------------------
import Text.HSpell.Base.Types
import Text.HSpell.Base.Parser

-- TODO: How to handle words with a hyphen? How about abreviations?
--
-- Tricky example of a single sentence:
--
-- "Hello Mr. Smith, you must've spent 4.2 dollars on this type-checker!"
--
-- This suggests we need to use something to handle contractions and
-- abbreviations neatly; I will leave this for the future, once
-- we get the general tool going.
sentences :: HSpellParser [Sentence]
sentences = many oneSentence <* eof
  where
    oneSentence = do
      f      <- tokenWord
      middle <- many (tokenWord <|> tokenPunct)
      optional (hstoken ".")
      return (f:middle)

tokenize :: Tokenizer
tokenize t = case runParser sentences () "" t of
  Left  _ -> error "Should not return an error"
  Right r -> r



