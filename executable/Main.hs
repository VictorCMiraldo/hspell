module Main where

import qualified Data.Text as T

import System.Environment

import Text.HSpell.Syntax.Dict

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
