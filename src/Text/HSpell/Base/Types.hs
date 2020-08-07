module Text.HSpell.Base.Types
  ( -- * Locations and Sections
    Loc(..) , loc0 , sameLine , sameCol , sucLine , sucCol
  , Section , sectValid , sectDisj
    -- * Tokens and Type Signatures for dealing with them
  , Token(..) , Tokenizer
  , Sentence
    -- * Input Files
  , HSpellInFile , getFileSection , getFileSectionWithCtx
  , hspellInFileFromText
    -- * Re-exports for convenience
  , T.Text
  ) where

import qualified Data.Text  as T
import qualified Data.Array as A
--------------------------------
import Text.HSpell.Util

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
  { tText :: !T.Text
  , tSect :: !Section
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
type Sentence = [Token]

-- |The input file will be stored as an array from line numbers
-- to lines. This ensures we can easily fetch portions of suggestions
-- and we can display the contents of the file /as they are/ to the user.
type HSpellInFile = A.Array Int T.Text

hspellInFileFromText :: T.Text -> HSpellInFile
hspellInFileFromText t =
  let ls = T.lines t
   in A.array (0 , length ls - 1) $ zip [0..] ls

-- |Applies 'T.drop' to the first line and 'T.take' to the last.
--
-- > cutFirstLast 3 6 ["abcdefg"]           = ("abc", ["def"] , "g")
-- > cutFirstLast 3 6 ["abcdefg","abcdefg"] = ("abc", ["def","abcdef"], "g")
--
cutFirstLast :: Int -> Int -> [T.Text] -> (T.Text , [T.Text] , T.Text)
cutFirstLast _     _   []  = (T.empty , [] , T.empty)
cutFirstLast start end [t] =
  let (bef , r)   = T.splitAt start t
      (res , aft) = T.splitAt (end - start) r
   in (bef , [res] , aft)
cutFirstLast start end (t:ts) =
  case snoc ts of
    Nothing         -> error "unreachable; ts is not empty"
    Just (ts' , t') ->
      let (bef , r)  = T.splitAt start t
          (r' , aft) = T.splitAt end t'
       in (bef , r:ts' ++ [r'] , aft)

-- |Given some context lines to be displayed and a section, returns
-- @ctx@ lines before the section, the extracted section and @ctx@ lines
-- after the section. Take a file, with its line numbers annotated:
--
-- > 42: xxxxxxxxxx
-- > 43: aaaaaaaaaa
-- > 44: xxxOOOOyyy
-- > 45: bbbbbbbbbb
-- > 46: xxxxxxxxxx
--
-- Let the file above be @f@, calling @getFileSectionWithCtx 1 (Loc 44 3, Loc 44 8) f@
-- would return:
--
-- > getFileSectionWithCtx 1 (Loc 44 3, Loc 44 8) f
-- >  = (["aaaaaaaaaa"], ("xxx" , ["OOOO"], "yyy") , ["bbbbbbbbbb"])
--
getFileSectionWithCtx :: Int -- ^ @ctx@
                      -> Section
                      -> HSpellInFile
                      -> ([T.Text] , (T.Text , [T.Text] , T.Text) , [T.Text])
getFileSectionWithCtx ctx (start , end) f =
 let befLs = map ((A.!) f) ctxBef
     aftLs = map ((A.!) f) ctxAft
  in (befLs , cutFirstLast (lCol start) (lCol end) (map ((A.!) f) deltaL) , aftLs)
 where
   ctxBef = [ l | l <- [lLine start - ctx .. lLine start - 1] , l >= 0 ]
   ctxAft = [ l | l <- [lLine end + 1 .. lLine end + ctx]     , l <= snd (A.bounds f) ]
   
   deltaL = [lLine start .. lLine end]

-- |Returns the text enclosed within a given section of a file.
getFileSection :: Section -> HSpellInFile -> [T.Text]
getFileSection s f =
  let (_ , (_ , res , _) , _) = getFileSectionWithCtx 0 s f
   in res

{-
test :: HSpellInFile
test = A.array (0,4) . zip [0..]
     $ map T.pack
     $ [ "some testing text"
       , "that is supposed to span"
       , "multiple lines, just like a file"
       , "in this case, we have five lines"
       , "this is \\texttt{te} last one."
       ]

-}

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
