{-# LANGUAGE FlexibleContexts #-}
-- |Provides the Interface to spellcheck words in individual
-- sentences. It outputs a list of suggestions for words located
-- within an offset from the beginning of the sentence.
module Text.HSpell.Syntax where

import           Data.List (foldl')
import qualified Data.Set  as S

import Control.Monad.Reader

import Text.HSpell.Base
import Text.HSpell.Syntax.Dict        

-- |Makes a syntax suggestion for a token and a list of possible
-- replacements and their frequencies in the target language.
makeSyntaxSuggestFor :: Token -> S.Set Text -> Suggest
makeSyntaxSuggestFor t = Suggest (tSect t) 

-- |Spellchecks a single token. Outputs a suggestion when a misspell
-- is detected.
spellcheckToken :: (MonadReader Dict m)
                => SymSpellVerbosity -> Token -> m (Maybe Suggest)
spellcheckToken v t = fmap (makeSyntaxSuggestFor t)
                  <$> spellcheckWord v (tText t)
                  <$> ask

-- |Given a sentence, produces the list of syntax suggestions its tokens
spellcheckSentence :: (MonadReader Dict m)
                   => SymSpellVerbosity -> Sentence -> m [Suggest]
spellcheckSentence v s = foldl' (\g -> maybe g (:g)) []
                     <$> mapM (spellcheckToken v) s
