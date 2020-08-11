module HSpell.Env where

import Control.Monad.RWS
------------------
import Text.HSpell.Base
import Text.HSpell.Syntax.Dict
import Text.HSpell.Grammar.Rule
-------------------
import HSpell.Config
import HSpell.Options

-- |The environment we require to run:
data HSpellEnv = HSpellEnv
  { envInput   :: HSpellInFile
  , envGrammar :: GrammarRuleSet
  , envCLIOpts :: HSpellOptions
  , envConfig  :: HSpellConfig
  } 

-- TODO: instead of keeping just one dictionary we should keep
-- one that is loaded on startup and the user dictionary
-- separate; the user dictionary carry no frequency information.
-- and this should make it easer to implement the /insert/ and
-- /accept/ actions whilst still enabling the user to save their
-- dictionay after the session.

-- TODO: do we even need frequency information?

data HSpellSt = HSpellSt
  { stDict :: Dict
  }

type HSpellM = RWST HSpellEnv () HSpellSt

runHSpellM :: (Monad m)
           => HSpellEnv -> HSpellSt -> HSpellM m a -> m (a , HSpellSt)
runHSpellM env st f = fmap (\(a , b , _) -> (a , b)) $ runRWST f env st


{-

-- |The state we require to run
data HSpellSt  = HSpellSt
  {
  -- , stIgnores     :: [Text] -- we should keep track of the ignores to be able
                               -- to save them on the personal dict later.
  -- |Suggestions are stored with their selected substitution.
    stSuggestions :: [(Suggest , Maybe Subst)]
  }

type HSpellM = RWST HSpellEnv () HSpellSt

runHSpellM :: (Monad m) => HSpellEnv -> HSpellSt -> HSpellM m a -> m (a , HSpellSt)
runHSpellM env st f = fmap (\(a , b , _) -> (a , b)) $ runRWST f env st

suggestionText :: (Monad m) => Suggest -> HSpellM m [Text]
suggestionText (Suggest (s , e) _) = do
  inp <- asks envInput
  undefined
  
-}
