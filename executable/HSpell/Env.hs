module HSpell.Env where

import Text.HSpell.Base
import Text.HSpell.Syntax.Dict
-------------------
import HSpell.Config
import HSpell.Options

-- |The environment we require to run:
data HSpellEnv = HSpellEnv
  { envInput   :: HSpellInFile
  , envDict    :: Dict
  , envCLIOpts :: HSpellOptions
  , envConfig  :: HSpellConfig
  } 

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
