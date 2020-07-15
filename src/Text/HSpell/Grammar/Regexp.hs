module Text.HSpell.Grammar.Regexp where

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad.Writer

data Rule k t
  = Beginning
  | End
  | Satisfy (t -> Bool)
  | As (Rule k t) k
  | Seq     [Rule k t]
  | Choice  [Rule k t]

tk :: (Eq t) => t -> Rule k t
tk t = Satisfy ((==) t)

-- |A 'match' is an offset on the original list and a list
-- of matched identifiers, if any.
type Match k t = Writer [(k, Int)] Int

registerMatch :: k -> Int -> Match k t -> Match k t
registerMatch k ix m = tell [(k , ix)] *> m

combineMatch :: Match k t -> Match k t -> Match k t
combineMatch m1 m2 = do
  i <- m1
  j <- m2
  return (min i j)

matchSeq :: (MonadPlus f , Eq t) => [Rule k t] -> ([t] , Int) -> f (Match k t , ([t] , Int))
matchSeq []     (ts , ix) = return (return ix , (ts , ix)) -- unit of product is 1
matchSeq (r:rs) (ts , ix) = do
  (m , tsix')  <- match' r (ts , ix)
  (ms, tsix'') <- matchSeq rs tsix'
  return (combineMatch m ms , tsix'')

matchOr :: (MonadPlus f , Eq t) => [Rule k t] -> ([t] , Int) -> f (Match k t , ([t] , Int))
matchOr []        _         = empty -- unit of sum is 0
matchOr (r : rs)  (ts , ix) = match' r (ts , ix) <|> matchOr rs (ts , ix)


match' :: (MonadPlus f, Eq t) => Rule k t -> ([t] , Int) -> f (Match k t , ([t] , Int))
match' Beginning   (ts , ix)
  | ix == 0   = return (return ix , (ts , ix))
  | otherwise = empty
match' End         ([] , ix)
  = return (return ix , ([] , ix))
match' (Satisfy f) ((t : ts) , ix)
  | f t       = return (return ix , (ts , ix + 1))
  | otherwise = empty
match' (As r k)    (ts , ix) = match' r (ts , ix) >>= return . (registerMatch k ix *** id)
match' (Seq rs)    (ts , ix) = matchSeq rs (ts , ix)
match' (Choice rs) (ts , ix) = matchOr  rs (ts , ix)
match' _        _         = empty

match :: (MonadPlus f, Eq t) => Rule k t -> [t] -> f (Match k t , ([t] , Int))
match r ts = go 0 ts 
  where
    go ix []       = empty
    go ix (t : ts) = match' r ((t : ts) , ix) <|> go (ix + 1) ts

data VAR = B | C
  deriving (Eq , Show)

test :: Rule VAR Char
test = Choice [ Seq [ tk 'a' , Choice [ tk 'b' , tk 'B' ] `As` B , tk 'c' `As` C , End ]
              , Seq [ tk 'c' , tk 'c' `As` C ] ]

res :: String -> [Match VAR Char]
res str = map fst (match test str)

resm :: String -> Maybe (Match VAR Char)
resm str = fmap fst (match test str)


-- match :: RuleSet t -> [t] -> [Matches t]
-- match = undefined
