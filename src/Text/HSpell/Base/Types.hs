module Text.HSpell.Base.Types
  ( -- * Locations and Sections
    Loc(..) , loc0 , sameLine , sameCol , sucLine , sucCol
  , Section , sectValid , sectDisj
    -- * Tokens and Type Signatures for dealing with them
  , Token(..) , Tokenizer
  , Sentence
    -- * Re-exports for convenience
  , Text
  ) where

import Data.Text (Text)

-- |Specifies a location in a file
data Loc = Loc
  { lLine  :: !Int
  , lCol   :: !Int
  } deriving (Eq , Show , Ord)

-- |Initial location in a file
loc0 :: Loc
loc0 = Loc 0 0

-- |Returns whether two locations reference the same line.
sameLine :: Loc -> Loc -> Bool
sameLine l1 l2 = lLine l1 == lLine l2

-- |Returns whether two locations reference the same column.
sameCol :: Loc -> Loc -> Bool
sameCol l1 l2 = lCol l1 == lCol l2

-- |Adds one to the line counter
sucLine :: Loc -> Loc
sucLine l = l { lLine = lLine l + 1 }

-- |Adds one to the column counter
sucCol :: Loc -> Loc
sucCol l = l { lCol = lCol l + 1 }

-- |A section of a file is identified by two locations. The first
-- component represents the start of the section, which we assume
-- always comes /before/ the end, as specified in 'sectValid'
type Section = (Loc , Loc)

-- |Checks whether a section is /valid/, useful for testing.
sectValid :: Section -> Bool
sectValid (s, e) = s < e

-- |Returns whether two section are disjoint
sectDisj :: Section -> Section -> Bool
sectDisj (s0, e0) (s1 , e1) = e0 < s1 || e1 < s0

-- |Finally, we will be spellchecking and performing grammatical
-- suggestions on streams of 'Token's, where a token is a bunch
-- of 'Text'
data Token = Token
  { tText :: !Text
  , tSect :: !Section
  } deriving (Eq , Show)

-- |A 'Tokenizer' splits the input into a list of 'Sentences'
type Tokenizer = Text -> [Sentence]

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
type Sentence = [Token]

{- 
-- |The 'HSpellEnv' carries the necessary dictionaries
data HSpellEnv = HSpellEnv
  { envDict :: Dict
  -- , envGrammar :: Grammar
  }

-- |Finally, the top-level functionality needs to receive
-- all the relevant enviroment pieces.
type HSpellM = Reader HSpellEnv
-}
