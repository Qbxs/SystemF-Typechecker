module Main where

import TypeChecker hiding (term, main)
import Parser

-- REPL (sort of)
main :: IO ()
main = do
  t <- putStr "λF> " >> getLine
  case parseTerm t of
    Left err -> print err >> main
    Right term -> case evalTypeCheck term of
        Left err -> putStrLn ("Cannot infere type of " <> show term) >> print err >> main
        Right type' -> putStrLn (show term <> " : " <> show type') >> main
