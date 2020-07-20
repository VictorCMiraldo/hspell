module Main where

import           System.Environment
import           Options.Applicative

import qualified Data.Text as T

import           Text.HSpell.Syntax.Dict

data Options = Options
  { optsDicts     :: [FilePath]
  , optsPrefixLen :: Int
  , optsEditDist  :: Int
  } deriving (Eq , Show)

optsParser :: Parser Options
optsParser = Options
  <$> optsDictsParser
  <*> optsPrefixLenParser
  <*> optsEditDistParser

optsDictsParser :: Parser [FilePath]
optsDictsParser = option _ $
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



main :: IO ()
main = do args <- getArgs
          case args of
            [word] -> checkSpelling word
            _      -> putStrLn "I need a word"

checkSpelling :: String -> IO ()
checkSpelling word = do
  dict <- largeDict1
  let res = spellcheck (T.pack word) dict
  putStrLn (show res)
