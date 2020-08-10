module Text.HSpell.Grammar.Rule where

import qualified Data.Text as T

import Text.HSpell.Base
import Text.HSpell.Grammar.Matcher

-- |Encodes a grammatical suggestion; examples include:
--
-- > grammar:
-- >   match: in order to
-- >   correct: to
-- >   descr: Using 'in order to' is pleonasm.
--
-- > grammar:
-- >   match: good in \1@\noum
-- >   correct: good at \1
-- >   descr: If you mean skilled in \1, use "at".
--
data GrammarRule = GrammarRule
  { grMatch       :: Rule Int T.Text
  , grSuggestion  :: T.Text
  , grDescription :: T.Text
  }
