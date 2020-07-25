module Text.HSpell.Base where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Control.Monad.Reader

import Text.HSpell.Syntax.Dict

-- |Adds locations data to some token type.
data Loc t = Loc
  { lLine  :: !Int
  , lCol   :: !Int
  , lToken :: t
  } deriving (Eq , Show)

-- |A 'Tokenizer' splits the input into a list of 'Sentences'
type Tokenizer = T.Text -> [Sentence]

-- |A 'Sentence' is a series of tokens with locations. We can't simply
-- ignore their locations because some file formats contain
-- bits and pieces that should not be spellchecked. Take
-- the following sentence in a tex file:
--
-- > 34: Some long sentence that spans
-- > 35: multiple \emph{lines}.
--
-- The tokenizer for tex should give us:
--
-- > [Loc 34 0 "Some" , Loc 34 6 "long" , Loc 34 11 "sentence"
-- > ,Loc 34 20 "that" , Loc 34 25 "spans" , Loc 35 0 "multiple"
-- > ,Loc 35 15 "lines" , Loc 35 21 "."]
--
type Sentence = [Loc (T.Text)]

-- |The 'HSpellEnv' carries the necessary dictionaries
data HSpellEnv = HSpellEnv
  { envDict :: Dict
  -- , envGrammar :: Grammar
  }

-- |Finally, the top-level functionality needs to receive
-- all the relevant enviroment pieces.
type HSpellM = Reader HSpellEnv
