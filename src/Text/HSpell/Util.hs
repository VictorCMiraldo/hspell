
module Text.HSpell.Util where

import qualified Data.Text as T

-- |@deletes xs@ generates a list of lists that correspond to one deletion
-- from @xs@.
deletes :: [a] -> [[a]]
deletes []     = []
deletes (x:xs) = xs : map (x:) (deletes xs)

-- |Generates all permutations within @n@ deletions from the original input.
deletesN :: Int -> [a] -> [[a]]
deletesN 0 _  = []
deletesN n xs = let dxs = deletes xs
                 in dxs ++ concatMap (deletesN (n-1)) dxs

-- |Generates all deletions for a given word
textDeletesN :: Int -> T.Text -> [T.Text]
textDeletesN i = map T.concat . deletesN i . T.chunksOf 1 

