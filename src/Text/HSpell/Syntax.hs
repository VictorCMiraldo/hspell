{-# LANGUAGE FlexibleContexts #-}
-- |Provides the Interface to spellcheck words in individual
-- sentences. It outputs a list of suggestions for words located
-- within an offset from the beginning of the sentence.
module Text.HSpell.Syntax where

import           Data.List (foldl')
import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.Char as C

import Control.Monad.Reader

import Text.HSpell.Base
import Text.HSpell.Syntax.Dict

-- TODO: We can surely implement a much better /case detection/
-- using edit distances: let misspelled be t and suggestion  be s;
-- let es = editscript (T.toLower t) s; then the copies where t
-- has an upper case letter also get translated to upper.

-- |Makes a syntax suggestion for a token and a list of possible
-- replacements and their frequencies in the target language.
makeSyntaxSuggestFor :: Token -> S.Set Text -> Suggest
makeSyntaxSuggestFor t = Suggest (tSect t) . fixCase
  where
    fixCase
      | C.isUpper (T.head $ tText t) = S.map mkFstUpper
      | otherwise                    = id

    mkFstUpper t0 = case T.uncons t0 of
      Nothing       -> error "makeSyntaxSuggestFor: empty suggestion"
      Just (c , t') -> T.cons (C.toUpper c) t'

-- |Spellchecks a single token. Outputs a suggestion when a misspell
-- is detected.
spellcheckToken :: Dict -> SymSpellVerbosity -> Token -> Maybe Suggest
spellcheckToken d v t =
  case tType t of
    TT_Word -> fmap (makeSyntaxSuggestFor t)
               $ spellcheckWord v (T.toLower $ tText t)
               $ d
    _       -> Nothing
 
