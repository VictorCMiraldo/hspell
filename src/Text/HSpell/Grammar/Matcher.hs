-- |Provides a polymorphic recursive linear regexp DSL to match
-- gramatical rules. Examples include, for instance, matching
-- on duplicate words:
--
-- > r :: Rule
-- > r = Seq [ anytk `As` 0 , var 0 ]
--
-- The @r@ above matches any token, stores it 'As' @0@, then matches
-- whatever is stored in variable @0@.
module Text.HSpell.Grammar.Matcher
  ( Rule(..)
  , tk , anytk , var
  , match , Match
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

-- |A rule matching on tokens of type @t@ and registering
-- variables of type @k@. It is essentially a linear
-- regular expression (no Kleene Closures).
data Rule k t
  -- |Matches the beginning of the input
  = Beginning 
  -- |Matches the end of the input
  | End
  -- |In case the rule matches successfully, stores the first token
  -- that witnesses the match with a given name.
  | As (Rule k t) k
  -- |Matches a token that satisfies a given condition; note that we
  -- have access to the list of matched variables so far. See 'tk', 'anytk'
  -- or 'var' for examples on how to use this.
  | Satisfy (t -> Reader [(k , t)] Bool) -- TODO: enable exceptions on this guy?
  -- |Matches a sequence of rules
  | Seq      [Rule k t]
  -- |Matches one of the given rules
  | Choice   [Rule k t]
  -- |Optionally matches on a rule
  | Optional (Rule k t)

-- |Matches a single token
tk :: (Eq t) => t -> Rule k t
tk t = Satisfy (return . (==) t)

-- |Matches any token
anytk :: Rule k t
anytk = Satisfy (return . const True)

-- |Matches a previously matched token
var :: (Eq t , Eq k) => k -> Rule k t
var k = Satisfy (\t -> (maybe False (== t) . lookup k) <$> ask)

-- |Returns whether or not the rule must start matching at the 'Beginning'
ruleStartsAt0 :: Rule k t -> Bool
ruleStartsAt0 Beginning   = True
ruleStartsAt0 (Seq (r:_)) = ruleStartsAt0 r
ruleStartsAt0 (Choice rs) = all ruleStartsAt0 rs
ruleStartsAt0 _           = False

-- |A 'matchSt is an offset on the original list and a list
-- of matched identifiers, if any.
type Match k t = (Int , [(k , t)])

-- |Matches a sequence of rules
matchSeq :: (MonadPlus f , Eq t) => [Rule k t] -> ([t] , Int)
         -> StateT [(k , t)] f (Int , ([t] , Int))
matchSeq []     (ts , ix) = return (ix , (ts , ix)) -- unit of product is 1
matchSeq (r:rs) (ts , ix) = do
  (m , tsix')  <- matchSt r (ts , ix)
  (_ , tsix'') <- matchSeq rs tsix'
  return (m , tsix'')

-- |Matches one of the given rules
matchOr :: (MonadPlus f , Eq t) => [Rule k t] -> ([t] , Int) -> StateT [(k , t)] f (Int , ([t] , Int))
matchOr []        _         = empty -- unit of sum is 0
matchOr (r : rs)  (ts , ix) = matchSt r (ts , ix) <|> matchOr rs (ts , ix)

matchSt :: (MonadPlus f, Eq t) => Rule k t -> ([t] , Int)
        -> StateT [(k , t)] f (Int , ([t] , Int))
matchSt Beginning   (ts , ix)
  | ix == 0   = return (ix , (ts , ix))
  | otherwise = empty
matchSt End         ([] , ix)
  = return (ix , ([] , ix))
matchSt (Satisfy f) ((t : ts) , ix) = do
  st <- get
  if runReader (f t) st
  then return (ix , (ts , ix + 1))
  else empty
-- TODO: this is nasty... what if the rule matches a large portion?
-- why are we only storing the first matched token
matchSt (As r k)    (ts@(t:_) , ix) = do
  res <- matchSt r (ts , ix)
  modify ((k , t):)
  return res
matchSt (Seq rs)     (ts , ix) = matchSeq rs (ts , ix)
matchSt (Choice rs)  (ts , ix) = matchOr  rs (ts , ix)
matchSt (Optional r) (ts , ix) = matchSt r (ts , ix) <|> return (ix , (ts , ix))
matchSt _        _             = empty

match' :: (MonadPlus f, Eq t) => Rule k t -> ([t] , Int) -> f (Match k t)
match' r inp = fmap (\((i , _) , vs) -> (i , vs)) $ runStateT (matchSt r inp) []

match :: (MonadPlus f, Eq t) => Rule k t -> [t] -> f (Match k t)
match r ts0 
  | ruleStartsAt0 r = match' r (ts0 , 0)
  | otherwise       = go 0 ts0
  where
    go _  []       = empty
    go ix (t : ts) = match' r ((t : ts) , ix) <|> go (ix + 1) ts

{-
--

test :: Rule Int Char
test = Choice [ Seq [ anytk , Choice [ tk 'b' , tk 'B' ] `As` 0 , tk 'c' `As` 1 , End ]
              , Seq [ tk 'c' , anytk `As` 2 ] ]

test2 :: Maybe (Match k String)
test2 = let r = Seq $ map tk [ "in" , "order" , "to" ]
         in match r (words "we will do x in order to explain y")

test3 :: [Match Int String]
test3 = let r = Seq [ anytk `As` 0 , Optional (tk ",") , var 0 ]
         in match r (words "tomorrow we we will we , we see on a very long sentence but it really seems like like performance wont be a significant issue")

test4 :: [Match Int String]
test4 = let r = Seq [ anytk `As` 0 , Optional (tk "," `As` 1) , Choice [var 0 , var 1] ]
         in match r (words "a , , b")






res :: String -> [Match Int Char]
res str = match test str

resm :: String -> Maybe (Match Int Char)
resm str = match test str


-- match :: RuleSet t -> [t] -> [Matches t]
-- match = undefined

-}
