module Text.HSpell.Grammar.Rule where

import qualified Data.Text as T

import Text.HSpell.Grammar.Matcher

-- |Encodes a grammatical suggestion; examples include:
--
-- > grammar:
-- >   match: in order to
-- >   correct: to
-- >   descr: Using 'in order to' is pleonasm.
--
-- > grammar:
-- >   match: good in \noum@n
-- >   correct: good at \n
-- >   descr: If you mean skilled in \n, use "at".
--
data GrammarRule = GrammarRule
  { grMatch       :: Rule Int T.Text
  , grSuggestion  :: T.Text
  , grDescription :: T.Text
  }
