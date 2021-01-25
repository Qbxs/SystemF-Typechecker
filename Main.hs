module Main where

import System.Console.ANSI
import Control.Monad (when)
import System.Exit (exitSuccess)
import TypeChecker hiding (term, main)
import Parser


main :: IO ()
main = do
  setTitle "SystemF - Repl"
  loop

-- | REPL (sort of)
loop :: IO ()
loop = do
  colorize Blue $ putStr "λF> "
  t <- getLine
  when (t == ":q") $ putStrLn "Bye Bye!" >> exitSuccess
  case parseTerm t of
    Left err -> print err >> loop
    Right term -> case evalTypeCheck term of
        Left err -> do
          putStrLn ("Cannot infere type of " <> show term)
          colorize Red $ print err
          loop
        Right type' -> do
          putStr $ show term
          colorize Green $ putStr " : "
          print type'
          loop

-- | Set and reset color for a given IO-action
colorize :: Color -> IO () -> IO ()
colorize c m = setSGR [SetColor Foreground Vivid c] >> m >> setSGR [Reset]
