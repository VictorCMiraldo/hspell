module Main where

import           System.Exit

import           Control.Monad.Reader

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.HSpell.Base
import           Text.HSpell.Tokenizer.Plain
import           Text.HSpell.Syntax
import           Text.HSpell.Syntax.Dict
import           Text.HSpell.Syntax.Dict.Parser

import HSpell.Config
import HSpell.Options
import HSpell.Env
import HSpell.Interface.Suggestion

main :: IO ()
main = do
  opts         <- processCLIOptions
  let confFile = maybe "defaultHSpellConfig" id $ optsConfig opts
  mconf        <- loadConfigFromFile confFile
  case mconf of
    Nothing   -> putStrLn ("Couldn't load " ++ show confFile) >> exitWith (ExitFailure 1)
    Just conf -> mainWithConf opts conf >>= exitWith

-- TODO: Combine opts and conf into an execution enviroment
mainWithConf :: HSpellOptions -> HSpellConfig -> IO ExitCode
mainWithConf opts conf = do
  dict <- loadSyntaxDictionary conf
  inp  <- T.readFile (optsWorkOnFile opts)
  runReaderT (mainWithEnv inp) $ HSpellEnv
    (hspellInFileFromText inp) dict opts conf
  
responseToSubst :: Suggest -> SugResult -> IO [Subst]
responseToSubst _ SugAccept         = putStrLn "Accept is not yet implemented" >> return [] 
responseToSubst _ SugInsert         = putStrLn "Insert is not yet implemented" >> return []
responseToSubst s (SugReplaceFor t) = return [Subst (sugSection s) t]

-- TODO: run by induction on sentences; then on tokens.
-- After spell checking an entire sentence, grammar-check it.
-- 
mainWithEnv :: Text -> ReaderT HSpellEnv IO ExitCode
mainWithEnv inp = do
  dict <- asks envDict
  opts <- asks envCLIOpts
  let tks  = tokenize inp
  let sugs = flip runReader dict
           $ mapM (spellcheckSentence (optsVerb opts)) tks
  eres <- sequence <$> mapM askSuggestion (concat sugs)
  case eres of
    Left err  -> lift $ putStrLn err >> return (ExitFailure 1)
    Right res -> do
      ss   <- concat <$> (lift $ mapM (uncurry responseToSubst) $ zip (concat sugs) res)
      file <- asks envInput
      lift $ mapM_ (putStrLn . T.unpack) $ executeSubsts ss file
      return ExitSuccess
  
loadSyntaxDictionary :: HSpellConfig -> IO Dict
loadSyntaxDictionary conf = do
  let dconf = DictConfig (dictMaxDist conf) (dictPrefixLength conf)
  combineDicts dconf <$> mapM (loadDictFromFile dconf) (dictionaries conf)

