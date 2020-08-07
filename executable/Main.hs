module Main where

import           System.Exit

import           Control.Monad.Reader

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
    Just conf -> mainWithConf opts conf

-- TODO: Combine opts and conf into an execution enviroment

mainWithConf :: HSpellOptions -> HSpellConfig -> IO ()
mainWithConf opts conf = do
  dict <- loadSyntaxDictionary conf
  inp  <- T.readFile (optsWorkOnFile opts)
  runReaderT (mainWithEnv inp) $ HSpellEnv
    (hspellInFileFromText inp) dict opts conf
  

mainWithEnv :: Text -> ReaderT HSpellEnv IO ()
mainWithEnv inp = do
  dict <- asks envDict
  opts <- asks envCLIOpts
  let tks  = tokenize inp
  let sugs = flip runReader dict
           $ mapM (spellcheckSentence (optsVerb opts)) tks
  res <- mapM askSuggestion (concat sugs)
  lift $ mapM_ (putStrLn . show) res
  
loadSyntaxDictionary :: HSpellConfig -> IO Dict
loadSyntaxDictionary conf = do
  let dconf = DictConfig (dictMaxDist conf) (dictPrefixLength conf)
  combineDicts dconf <$> mapM (loadDictFromFile dconf) (dictionaries conf)

