module Main where

import           System.Environment
import           Options.Applicative

import qualified Data.Text as T

import           Text.HSpell.Syntax.Dict
import           Text.HSpell.Syntax.Parser

data Options = Options
  { optsDicts     :: FilePath
  , optsPrefixLen :: Int
  , optsEditDist  :: Int
  , optsWord      :: String
  } deriving (Eq , Show)

optsParser :: Parser Options
optsParser = Options
  <$> optsDictsParser
  <*> optsPrefixLenParser
  <*> optsEditDistParser
  <*> argument str (metavar "Word")

-- TODO: Parse a list of dictionaries? What will the user interface
-- look like?
optsDictsParser :: Parser FilePath
optsDictsParser = option str $
     long "dicts"
  <> short 'd'
  <> metavar "FILE,FILE,..."
  <> help "List of dictionaries to load"

optsPrefixLenParser :: Parser Int
optsPrefixLenParser = option auto $
     long "prefix-len"
  <> short 'p'
  <> showDefault
  <> value 4
  <> metavar "INT"
  <> help "Length of prefix to precompute deletions for."

optsEditDistParser :: Parser Int
optsEditDistParser = option auto $
     long "edit-dist"
  <> short 'e'
  <> showDefault
  <> value 2
  <> metavar "INT"
  <> help "Maximum Damerau-Leveshtein distance to suggest spelling corrections."

optsParserInfo :: ParserInfo Options
optsParserInfo = info optsParser $
  fullDesc <> progDesc "Spells checks things"
           <> header "hpsell"

main :: IO ()
main = do
  options <- customExecParser p optsParserInfo
  checkspelling options
 where
   p = prefs showHelpOnEmpty

checkspelling :: Options -> IO ()
checkspelling opts = do
  let dc = DictConfig (optsEditDist opts) (optsPrefixLen opts)
  dict <- loadDictFromFile dc (optsDicts opts)
  let res = spellcheck (T.pack $ optsWord opts) dict
  putStrLn (show res)

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

