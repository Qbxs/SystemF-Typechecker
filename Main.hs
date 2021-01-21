module Main where

import TypeChecker hiding (term, main)
import Parser

main :: IO ()
main = do
  t <- getLine
  case parseTerm t of
    Left err -> print err >> main
    Right term -> do
      print term
      print $ runTypeCheck term
