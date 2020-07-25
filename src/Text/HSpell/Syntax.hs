-- |Provides the interface to spellcheck words in individual
-- sentences. It outputs a list of suggestions for words located
-- within an offset from the beginning of the sentence.
module Text.HSpell.Syntax where

import qualified Data.Text as T
import qualified Data.Set  as S

import Control.Monad.Reader

import Text.HSpell.Base
import Text.HSpell.Syntax.Dict        
import Text.HSpell.Syntax.Dict.Parser

data SyntaxSuggestion = SyntaxSuggestion
  { ssOffset       :: Int
  , ssAlternatives :: S.Set T.Text
  }

spellcheck :: Sentence -> HSpellM [SyntaxSuggestion]
spellcheck sent = asks envDict >>= return . go 0 sent
  where
    go _  []     _ = []
    go ix (w:ws) d =
      let rest = go (ix + 1) ws d
       in maybe rest (\s -> SyntaxSuggestion ix s : rest)
        $ spellcheckWord (lToken w) d
