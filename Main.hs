module Main where

import System.Console.ANSI
import TypeChecker hiding (term, main)
import Parser


main :: IO ()
main = do
  setTitle "SystemF - Repl"
  loop

-- | REPL (sort of)
loop :: IO ()
loop = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr "λF> "
  setSGR [Reset]
  t <- getLine
  case parseTerm t of
    Left err -> print err >> loop
    Right term -> case evalTypeCheck term of
        Left err -> do
          putStrLn ("Cannot infere type of " <> show term)
          setSGR [SetColor Foreground Vivid Red]
          print err
          setSGR [Reset]
          loop
        Right type' -> do
          putStr $ show term
          setSGR [SetColor Foreground Vivid Green]
          putStr " : "
          setSGR [Reset]
          print type'
          loop
