-- |When chekcing a file, the program will present the user with
-- suggestions, which might be taken into account or not. 
--
-- Here we define the mechanisms for dealing with suggestions
-- and later applying them to files.
module Text.HSpell.Base.Suggestion where

import qualified Data.Text  as T
import qualified Data.Set   as S
-------------------------------
import Text.HSpell.Base.Types

-- |Provides an abstract functionality for resolving sugestions,
-- the idea being that given a sentence @txt = "a b c"@; and a suggestion
-- that says, for instance, @s = SyntaxSug 1 ["B", "bb"]@, we will output
-- 'Nothing' when we want to ignore the suggestion; or a substitution.
-- Say we chose for "bb", the output of @resolveSynSug s@ should
-- be @Just (Subst (Loc x 2, Loc x 3) "bb")@, where @x@ here is whathever
-- line we are at. 
class MonadSuggest m where
  resolveSuggestion :: Suggest -> m (Maybe Subst)

  -- TODO: lots of things!
  -- ignoreSuggestion :: either Always Session Once -> Suggest -> m ()
  -- ...

-- |A 'Suggest' presents the user with a number of options
-- for changing a 'Section' of the file.
data Suggest = Suggest
  { sugSection      :: Section
  , sugAlternatives :: S.Set Text
  } deriving Show

-- |As the user makes decisions about which suggestions to
-- adopt, we record them in as 'Substitution's to be performed.
--
-- Say the user typed @eam@ in line 42, col 15, but they actually meant
-- @exam@. The instruction to perform that very modification would be
--
-- > Subst (Loc 42 15, Loc 42 17) "exam"
--
data Subst = Subst Section Text

-- |Executes a sequence of substitutions
executeSubsts :: [Subst] -> [Text] -> [Text]
executeSubsts s = applyLineOps (substsToLineOps s)

-- |A 'LineOp' describes an operation to be performed within a line.
-- Take the following three lines as an example:
--
-- > l1 = "Some optional text in a line"
-- > l2 = "Some text within a line"
-- > l3 = "Some text"
--
-- Transforming @l1@ into @l2@ consists in erasing @optional@,
-- or, as a 'LineOp': @LO_Subst 5 13 "" LO_Keep@.
-- 
-- Transforming @l2@ into @l3@, in turn, consists in erasing the
-- /rest of the line/ after text, or: @LO_Rest 10 ""@.
data LineOp
  -- |Substitutes a part of a line for some given text.
  = LO_Subst Int Int T.Text LineOp
  -- |Substitute from a point to the end of the line for some text.
  | LO_Rest Int T.Text
  -- |Keep the line as it is.
  | LO_Keep
  deriving (Eq , Show)

-- |Appends two 'LineOp's. We assume that the first argument operates over on a prefix
-- of the line that is untouched by the second argument.
lopAppend :: LineOp -> LineOp -> LineOp
lopAppend LO_Keep             op' = op'
lopAppend (LO_Subst i j t op) op' = LO_Subst i j t (lopAppend op op')
lopAppend (LO_Rest  i t) _        = LO_Rest i t

-- |Semantics of a 'LineOp' as a function over a /line/ of text.
applyLineOp :: LineOp -> T.Text -> T.Text
applyLineOp = go 0
 where
  go _  LO_Keep t = t
  go c (LO_Rest from repl) t
    = T.append (T.take (from - c) t) repl
  go c (LO_Subst from to repl ops) t
    = let (pref, rest) = T.splitAt (from - c) t
       in T.concat [ pref , repl , go (c + to - from) ops (T.drop (to - from) rest) ]

-- |A list of 'DelOrLineOp's, identified by which line they are supposed to modify,
-- can be applied to a list of lines.
applyLineOps :: [(Int , DelOrLineOp)] -> [T.Text] -> [T.Text]
applyLineOps = go 0
  where
    go :: Int -> [(Int , DelOrLineOp)] -> [T.Text] -> [T.Text]
    go _ _              []     = [] -- TODO: problems inserting at the end?
    go _ []             f      = f
    go c ((l , mop):ops) (t:ts)
      | c == l = case mop of
                   Nothing -> go (c+1) ops ts
                   Just op -> applyLineOp op t : go (c+1) ops ts
      | c < l     = t : go (c+1) ((l , mop):ops) ts
      | otherwise = error "Invariant broke: line operations not sorted by line"
     
-- |Sometimes we might need to delete entire lines, hence we'll be mostly
-- working with 'DelOrLineOp' where @Nothing@ represents /delete this line/.
type DelOrLineOp = Maybe LineOp

-- |A single 'Subst' can span accross many lines, hence providing a list of of
-- line operations, 'LineOp'
substToLineOp :: Subst -> [(Int , DelOrLineOp)]
substToLineOp (Subst (from, to) res)
  | sameLine from to = [(lLine from , Just $ LO_Subst (lCol from) (lCol to) res LO_Keep)]
  | otherwise        =  (lLine from , Just $ LO_Rest  (lCol from) res)
                      : [(i , Nothing) | i <- [lLine from + 1 .. lLine to - 1]]
                     ++ [(lLine to , Just $ LO_Subst 0 (lCol to) T.empty LO_Keep)]

-- |Given a list of /ordered/, /non-overlapping/ substitutions, we can produce a
-- list of 'DelOrLineOp'.
substsToLineOps :: [Subst] -> [(Int , DelOrLineOp)]
substsToLineOps = foldr (merge . substToLineOp) []
 where
   merge :: [(Int , DelOrLineOp)] -> [(Int , DelOrLineOp)] -> [(Int , DelOrLineOp)]
   merge ls            [] = ls
   merge []            ls = ls
   merge ((l , op):ls) ((l' , op'):ls') =
     case compare l l' of
       LT -> (l , op)   : merge ls ((l' , op'):ls')
       GT -> (l' , op') : merge ((l , op):ls) ls'
       EQ -> merge ((l , lopAppend <$> op <*> op'):ls) ls'

{-

TODO: Should files be arrays? It sure helps rendering the interface easily

import qualified Data.Array as A
     
type InFile = A.Array Int T.Text

test :: InFile
test = A.array (0,4) . zip [0..]
     $ map T.pack
     $ [ "some testing text"
       , "that is supposed to span"
       , "multiple lines, just like a file"
       , "in this case, we have five lines"
       , "this is \\texttt{te} last one."
       ]

testInstr :: [Subst]
testInstr = [ Subst (Loc 1 5 , Loc 1 7 ) (T.pack "should be")
            , Subst (Loc 2 14, Loc 3 14) (T.pack ".")
            , Subst (Loc 3 14, Loc 3 15) (T.pack "W")
            , Subst (Loc 4 16, Loc 4 18) (T.pack "the")
            , Subst (Loc 3 22, Loc 3 26) (T.pack "four")
            ]


t = execute (compile testInstr) (A.elems test)

-}
