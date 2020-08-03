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

-- |Provides an abstract functionality for resolving sugestions,
-- the idea being that given a sentence @txt = "a b c"@; and a suggestion
-- that says, for instance, @s = SyntaxSug 1 ["B", "bb"]@, we will output
-- 'Nothing' when we want to ignore the suggestion; or a substitution.
-- Say we chose for "bb", the output of @resolveSynSug txt s@ should
-- be @Just (Subst (Loc x 2, Loc x 3) "bb")@, where @x@ here is whathever
-- line we are at (can be extracted from the tokens in the 'Sentence').
class MonadSuggest m where
  resolveSuggestion :: Sentence -> Suggest -> m (Maybe Subst)

-- |A 'Suggest' presents the user with a number of options
-- for changing a 'Section' of the file.
data Suggest = Suggest
  { ssSection      :: Section
  , ssAlternatives :: S.Set Text
  }

-- |Makes a syntax suggestion for a token and a list of possible
-- replacements and their frequencies in the target language.
makeSyntaxSuggestFor :: Token -> S.Set (Text , Int) -> Suggest
makeSyntaxSuggestFor t = Suggest (tSect t) . S.map fst

-- |Spellchecks a single token. Outputs a suggestion when a misspell
-- is detected.
spellcheckToken :: (MonadReader Dict m) => Token -> m (Maybe Suggest)
spellcheckToken t = fmap (makeSyntaxSuggestFor t)
                <$> spellcheckWord (tText t)
                <$> ask

-- |Given a sentence, produces the list of syntax suggestions its tokens
spellcheckSentence :: (MonadReader Dict m) => Sentence -> m [Suggest]
spellcheckSentence s = foldl' (\g -> maybe g (:g)) []
                   <$> mapM spellcheckToken s
