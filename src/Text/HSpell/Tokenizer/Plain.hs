-- |Provides a simple tokenizer for plain text.
-- Sentences are split on full stops ('.').
module Text.HSpell.Tokenizer.Plain where

import Text.Parsec
-------------------------------
import Text.HSpell.Base.Types
import Text.HSpell.Base.Parser

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



