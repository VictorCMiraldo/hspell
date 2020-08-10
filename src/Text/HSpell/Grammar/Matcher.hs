{-# LANGUAGE ScopedTypeVariables #-}
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
  , parseRuleWith , parseWord , parseIntVar , parseSimpleRule
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import qualified Text.Parsec.Prim as P
import Text.Parsec.Combinator
import Text.Parsec.Char
import qualified Data.Text as T
--------------------------
import Text.HSpell.Base.Parser (HSpellParser, lexeme, word)


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

instance (Show k , Show t) => Show (Rule k t) where
  show Beginning    = "\\^"
  show End          = "\\$"
  show (As r n)     = show n ++ "@(" ++ show r ++ ")"
  show (Seq as)     = "(Seq " ++ show as ++ ")"
  show (Choice as)  = "(Choice " ++ show as ++ ")"
  show (Optional r) = show r ++ "\\?"
  show (Satisfy _)  = "TK"

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

-- |A 'matchSt is an offset on the original list, a number of matched
-- tokens and a list of matched identifiers, if any.
type Match k t = (Int , Int, [(k , t)])

-- TODO: Package all of this decently into a "Matcher" monad

-- |Matches a sequence of rules; returns the offset that started
-- the match and the number of  matched tokens
matchSeq :: (MonadPlus f , Eq t) => [Rule k t] -> ([t] , Int)
         -> StateT [(k , t)] f (Int , Int , ([t] , Int))
matchSeq []     (ts , ix) = return (ix , 0 , (ts , ix)) -- unit of product is 1
matchSeq (r:rs) (ts , ix) = do
  (m , c  , tsix')  <- matchSt r (ts , ix)
  (_ , c' , tsix'') <- matchSeq rs tsix'
  return (m , c + c' , tsix'')

-- |Matches one of the given rules
matchOr :: (MonadPlus f , Eq t) => [Rule k t] -> ([t] , Int) -> StateT [(k , t)] f (Int , Int , ([t] , Int))
matchOr []        _         = empty -- unit of sum is 0
matchOr (r : rs)  (ts , ix) = matchSt r (ts , ix) <|> matchOr rs (ts , ix)

matchSt :: (MonadPlus f, Eq t) => Rule k t -> ([t] , Int)
        -> StateT [(k , t)] f (Int , Int , ([t] , Int))
matchSt Beginning   (ts , ix)
  | ix == 0   = return (ix , 0 , (ts , ix))
  | otherwise = empty
matchSt End         ([] , ix)
  = return (ix , 0 , ([] , ix))
matchSt (Satisfy f) ((t : ts) , ix) = do
  st <- get
  if runReader (f t) st
  then return (ix , 1 , (ts , ix + 1))
  else empty
-- TODO: this is nasty... what if the rule matches a large portion?
-- why are we only storing the first matched token
matchSt (As r k)    (ts@(t:_) , ix) = do
  res <- matchSt r (ts , ix)
  modify ((k , t):)
  return res
matchSt (Seq rs)     (ts , ix) = matchSeq rs (ts , ix)
matchSt (Choice rs)  (ts , ix) = matchOr  rs (ts , ix)
matchSt (Optional r) (ts , ix) = matchSt r (ts , ix) <|> return (ix , 0 , (ts , ix))
matchSt _        _             = empty

match' :: (MonadPlus f, Eq t) => Rule k t -> ([t] , Int) -> f (Match k t)
match' r inp = fmap (\((i , e , _) , vs) -> (i , e , vs)) $ runStateT (matchSt r inp) []

match :: (MonadPlus f, Eq t) => Rule k t -> [t] -> f (Match k t)
match r ts0 
  | ruleStartsAt0 r = match' r (ts0 , 0)
  | otherwise       = go 0 ts0
  where
    go _  []       = empty
    go ix (t : ts) = match' r ((t : ts) , ix) <|> go (ix + 1) ts

-----------------------
-- * Parsing Rules * --
-----------------------

parseIntVar :: HSpellParser Int
parseIntVar = char '\\' >> (read <$> lexeme (many digit))

parseWord :: HSpellParser T.Text
parseWord = fst <$> word

parseRuleWith :: forall k t . (Eq t , Eq k)
              => HSpellParser k -> HSpellParser t -> HSpellParser (Rule k t)
parseRuleWith pVar pTk = pSum
  where
    fOnList _ [x] = x
    fOnList f xs  = f xs
    
    pSum = fOnList Choice <$> sepBy (lexeme pSeq) (lexeme $ char '|')
    
    pSeq = fOnList Seq <$> many1 pNamed

    pNamed = do
      mnamed <- optionMaybe (pVar <* char '@')
      atom   <- pAtom
      return $ case mnamed of
        Just name -> atom `As` name
        Nothing   -> atom

    pAtom :: HSpellParser (Rule k t)
    pAtom = choice [ pOptEscapedTk
                   , P.try (lexeme $ char '(') *> lexeme pSum <* lexeme (char ')')
                   ]

    pOptEscapedTk :: HSpellParser (Rule k t)
    pOptEscapedTk = do
      tk0 <- pEscapedTk
      choice [ P.try (char '\\' >> lexeme (char '?') >> return (Optional tk0))
             , return tk0 ]
    
    pEscapedTk :: HSpellParser (Rule k t)
    pEscapedTk = P.try $ (tk <$> pTk) P.<|> lexeme (do
      _ <- char '\\'
      choice [ P.try (char '.') >> return anytk
             , P.try (char '^') >> return Beginning
             , P.try (char '$') >> return End
             , var <$> pVar
             ])


parseSimpleRule :: HSpellParser (Rule Int T.Text)
parseSimpleRule = parseRuleWith parseIntVar parseWord

{-

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
-}
