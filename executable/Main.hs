module Main where

import           System.Exit

import           Control.Monad.Reader

import qualified Data.Text.IO as T

import           Text.HSpell.Tokenizer.Plain
import           Text.HSpell.Syntax
import           Text.HSpell.Syntax.Dict
import           Text.HSpell.Syntax.Dict.Parser

import Config
import Options

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
  let tks  = tokenize inp
  let sugs = flip runReader dict
           $ mapM (spellcheckSentence (dictVerbosity conf)) tks
  mapM_ (putStrLn . show) (concat sugs)
  
loadSyntaxDictionary :: HSpellConfig -> IO Dict
loadSyntaxDictionary conf = do
  let dconf = DictConfig (dictMaxDist conf) (dictPrefixLength conf)
  combineDicts dconf <$> mapM (loadDictFromFile dconf) (dictionaries conf)

{-
checkspelling :: Options -> IO ()
checkspelling opts = do
  let dc = DictConfig (optsEditDist opts) (optsPrefixLen opts)
  dict <- loadDictFromFile dc (optsDicts opts)
  let res = spellcheck (T.pack $ optsWord opts) dict
  putStrLn (show res)
-}

{-
checkSpelling :: String -> IO ()
checkSpelling word = do
  _
  {-
  dict <- largeDict
  let res = spellcheck (T.pack word) dict
  putStrLn (show res)
  -}
-}


{-

Here's some UI sketch; curses is pretty low-level... haha

import UI.NCurses
import UI.NCurses.Panel

data HSpellUIToken
  = TkBG    T.Text
  | TkFG    T.Text
  | TkFocus T.Text
  deriving (Eq , Show)

renderHSpellUI
  :: Int             -- ^ Current line number being spell-checked
  -> [HSpellUIToken] -- ^ List of tokens under spell-check
  -> [T.Text]        -- ^ List of suggestions
  -> T.Text          -- ^ Description of suggestion
  -> Curses Window
renderHSpellUI line tks sugs desc = do
  (wd,hg) <- screenSize
  
  

main :: IO ()
main = runCurses $ do
    setEcho False
    (width,height) <- screenSize
    
    w <- defaultWindow
    n <- newWindow 20 20 20 20
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    updateWindow n $ do
        moveCursor 0 0
        drawString "Test"
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

-}

