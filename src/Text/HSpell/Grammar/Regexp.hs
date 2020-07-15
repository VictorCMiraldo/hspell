module Text.HSpell.Grammar.Regexp where

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad.Writer

-- |A rule matching on tokens of type @t@ and registering
-- variables of type @k@ is essentially a linear regular expression,
-- that is, no Kleene Closures.
--
data Rule k t
  = Beginning
  | End
  | Satisfy (t -> Bool)
  | As (Rule k t) k
  | Seq     [Rule k t]
  | Choice  [Rule k t]

tk :: (Eq t) => t -> Rule k t
tk t = Satisfy ((==) t)

anytk :: Rule k t
anytk = Satisfy (const True)

ruleStartsAt0 :: Rule k t -> Bool
ruleStartsAt0 Beginning    = True
ruleStartsAt0 (Seq (r:rs)) = ruleStartsAt0 r
ruleStartsAt0 (Choice rs)  = all ruleStartsAt0 rs
ruleStartsAt0 _            = False

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
match r ts 
  | ruleStartsAt0 r = match' r (ts , 0)
  | otherwise       = go 0 ts
  where
    go ix []       = empty
    go ix (t : ts) = match' r ((t : ts) , ix) <|> go (ix + 1) ts

test :: Rule Char Char
test = Choice [ Seq [ anytk , Choice [ tk 'b' , tk 'B' ] `As` 'b' , tk 'c' `As` 'c' , End ]
              , Seq [ tk 'c' , anytk `As` 'c' ] ]

test2 :: Maybe (Match k String)
test2 = let r = Seq $ map tk [ "in" , "order" , "to" ]
         in fmap fst $ match r (words "we will do x in order to explain y")





res :: String -> [Match Char Char]
res str = map fst (match test str)

resm :: String -> Maybe (Match Char Char)
resm str = fmap fst (match test str)


-- match :: RuleSet t -> [t] -> [Matches t]
-- match = undefined
