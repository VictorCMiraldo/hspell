module Text.HSpell.Base where

import           Data.List (foldl')
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.Array as A

import Control.Monad.Reader
import Control.Monad.State

{-

      | c == from = maybe T.empty id repl
      | c == from = T.append (maybe T.empty id repl) (execLine' ops dropTil 
executeSubsts :: [Subst] -> [T.Text] -> [T.Text]
executeSubsts s f = evalState (exec s) ((0 , 0) , f)
  where
    exec :: [Subst] -> State ((Int , Int) , [T.Text]) [T.Text]
    exec []     = _
    exec (s:ss) = _

    copyUntil :: Loc () -> State ((Int , Int) , [T.Text]) [T.Text]
    copyUntil dest = do
      ((l , c) , f) <- get
      

    exec1 :: Subst -> State ((Int , Int) , [T.Text]) ([T.Text] , T.Text)
    exec1 (Subst from to res) = do
      prefix <- copyUntil from 
      skipUntil to
      return (prefix , res)

-}
    


{-

-- |A 'LineSubst' is a /lower level/ version of subst; the user
-- will generate a list of 'Subst' when interacting with the
-- checker. This list is later compiled to a list of line operations
-- /per line/, which is then finally executed to produce the final file.
data LineOp
  = RmLine
  | ToEnd Int T.Text
  | Between Int Int T.Text
  deriving (Eq, Show)

substToLineOp :: Subst -> [(Int , LineOp)]
substToLineOp (Subst from to res)
  | sameLine from to = [(lLine from , Between (lCol from) (lCol to) res)]
  | otherwise =
    

{-
-- |Given a sorted list of substitutions and the /source/ file, compute
-- the /destination/ file.
subst :: [Subst] -> InFile -> [T.Text]
subst s = go (compile 0 s) 0
  where
    -- Compiles a list of instructions into a list of 
    compile :: Int -> [Subst] -> [Maybe [(Int , Int)]]
    go :: [Subst] -> Int -> InFile -> [T.Text]
    go [] _ src = src
    go (Subst from to tgt : ss) l fs
      | sameLine from to = 
          case delInterval (lCol from) (lLine to - lLine from , lCol to) fs of
            Just (g , gs) -> go ss _ (g:gs)
-}


{-

-- abcdefg                         abcdefg
-- ijklmno  -- (1,4) to (2,4) -->  ijkl
-- pqrstuv                         tuv
delInterval :: Int -> (Int , Int) -> [T.Text] -> Maybe (T.Text, [T.Text])
delInterval _  _         []     = Nothing
delInterval j0 (0  , j1) (x:xs) =
  let (a , b) = T.splitAt j0 x
      (_ , r) = T.splitAt (j1 - j0) b
   in Just (T.concat [a , r] , xs)
delInterval j0 (i1 , j1) (x:xs) =
  case drop (i1 - 1) xs of
   (r:rs) -> Just (T.take j0 x , T.drop j1 r : rs)
   []     -> Nothing


-}

-}
