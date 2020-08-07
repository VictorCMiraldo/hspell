
module Text.HSpell.Util where

import           Control.Arrow ((***))
import qualified Data.Text as T
import           Data.List (sort)

-- |@deletes xs@ generates a list of lists that correspond to one deletion from @xs@.
deletes :: [a] -> [[a]]
deletes []     = []
deletes (x:xs) = xs : map (x:) (deletes xs)

-- |Generates a list of lists that corresponds to exacly @n@
-- deletions from the input list.
deletesN :: (Ord a) => Int -> [a] -> [[a]]
deletesN n0 = nub2 . sort . dels' n0
  where
    nub2 :: Ord a => [a] -> [a]
    nub2 []  = []
    nub2 [x] = [x]
    nub2 (x:y:ys)
      | x == y     = nub2 (y:ys)
      | otherwise  = x : nub2 (y:ys)
    
    dels' :: Int -> [a] -> [[a]]
    dels' _ []     = []
    dels' 1 xs     = deletes xs
    dels' n (x:xs) = map (x:) (dels' n xs) ++ dels' (n-1) xs

-- |Generates a list of lists that corresponds to up to
-- @n@ deletions.
deletesUpToN :: (Ord a) => Int -> [a] -> [[a]]
deletesUpToN n xs = concatMap (flip deletesN xs) [1..n]

-- |Generates all deletions for a given word
textDeletesN :: Int -> T.Text -> [T.Text]
textDeletesN i = map T.concat . deletesUpToN i . T.chunksOf 1 


snoc :: [a] -> Maybe ([a] , a)
snoc []     = Nothing
snoc [a]    = Just ([] , a)
snoc (x:xs) = fmap ((x:) *** id) $ snoc xs
