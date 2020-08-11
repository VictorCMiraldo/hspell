module Main where

import           System.Exit

import           Control.Arrow ((***))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Maybe (mapMaybe)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.HSpell.Base
import           Text.HSpell.Tokenizer.Plain
import           Text.HSpell.Syntax
import           Text.HSpell.Syntax.Dict
import           Text.HSpell.Syntax.Dict.Parser
import           Text.HSpell.Grammar.Rule

import HSpell.Config
import HSpell.Options
import HSpell.Env
import HSpell.Interface.Suggestion

main :: IO ()
main = do
  opts         <- processCLIOptions
  let confFile = maybe "config.yaml" id $ optsConfig opts
  mconf        <- loadConfigFromFile confFile
  case mconf of
    Left err   -> putStrLn ("Couldn't load: " ++ show confFile ++ "; " ++ show err)
               >> exitWith (ExitFailure 1)
    Right conf -> mainWithConf opts conf >>= exitWith

-- TODO: Combine opts and conf into an execution enviroment
mainWithConf :: HSpellOptions -> HSpellConfig -> IO ExitCode
mainWithConf opts conf = do
  dict <- loadSyntaxDictionary conf
  gr   <- loadGrammarRules conf
  inp  <- T.readFile (optsWorkOnFile opts)
  let env = HSpellEnv (hspellInFileFromText inp) gr opts conf
  let st  = HSpellSt dict
  fst <$> runHSpellM env st (hspellMain inp)
  
loadSyntaxDictionary :: HSpellConfig -> IO Dict
loadSyntaxDictionary conf = do
  let dconf = DictConfig (dictMaxDist conf) (dictPrefixLength conf)
  combineDicts dconf <$> mapM (loadDictFromFile dconf) (dictionaryFiles conf)

loadGrammarRules :: HSpellConfig -> IO GrammarRuleSet
loadGrammarRules conf = do
  mres <- sequence <$> mapM loadRulesFromFile (grammarRuleFiles conf)
  case mres of
    Left err  -> putStrLn (show err) >> exitWith (ExitFailure 1)
    Right res -> return $ concat res

--------------------------

-- After loading all the configuration; we finally run the
-- operational main
hspellMain :: Text -> HSpellM IO ExitCode
hspellMain t = do
  s <- runExceptT (goSentences (tokenize t))
  case s of
    Left err  -> lift $ putStrLn err >> return (ExitFailure 1)
    Right res -> do
      file <- asks envInput
      lift $ mapM_ (putStrLn . T.unpack) $ executeSubsts res file
      return ExitSuccess

goSentences :: [Sentence] -> ExceptT String (HSpellM IO) [Subst]
goSentences []     = return []
goSentences (s:ss) = do
  (s' , spellSubsts) <- ioSpellcheck s
  gramSubsts         <- ioGrammcheck s'
  rest               <- goSentences ss
  return $ spellSubsts ++ gramSubsts ++ rest
  
ioSpellcheck :: Sentence -> ExceptT String (HSpellM IO) (Sentence , [Subst])
ioSpellcheck s = (id *** mapMaybe id) . unzip <$> mapM ioSpellcheckTk s

ioSpellcheckTk :: Token -> ExceptT String (HSpellM IO) (Token , Maybe Subst)
ioSpellcheckTk tk = do
  verb <- optsVerb <$> asks envCLIOpts
  dict <- gets stDict
  case spellcheckToken dict verb tk of
    Nothing  -> return (tk , Nothing)
    Just sug -> do
      res <- ExceptT $ askSuggestion sug
      case res of
        -- TODO: maintain list of accepted tokens during session
        SugAccept          -> return (tk , Nothing)
        SugInsert          -> throwError "insert is not yet implemented"
        SugReplaceFor txt  -> return ( tk { tText = txt }
                                     , Just $ Subst (sugSection sug) txt)

ioGrammcheck :: Sentence -> ExceptT String (HSpellM IO) [Subst]
ioGrammcheck s = do
  gr <- asks envGrammar
  let sugs = grammarcheck gr s
  concat <$> (forM sugs $ \s0 -> do
    res <- ExceptT $ askSuggestion s0
    case res of
        SugAccept          -> return []
        SugReplaceFor txt  -> return $ [Subst (sugSection s0) txt]
        _ -> throwError "Only replace for and accept are implemented for grammar checking")

