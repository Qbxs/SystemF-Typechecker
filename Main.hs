module Main where

import System.Console.ANSI
import Control.Monad.State
import qualified Data.Map as M
import System.Exit (exitSuccess)
import TypeChecker hiding (term, main)
import Parser


main :: IO ()
main = do
  setTitle "SystemF - Repl"
  evalStateT loop richCtx

-- | REPL with variable context
loop :: StateT (M.Map String Type) IO ()
loop = do
  lift $ colorize Blue $ putStr "λF> "
  str <- lift getLine
  ctx <- get
  case parseStmt str of
    Left err -> lift (print err) >> loop
    Right stmt -> evalStmt stmt >> loop

-- | Set and reset color for a given IO-action
colorize :: Color -> IO () -> IO ()
colorize c m = do
  setSGR [SetColor Foreground Vivid c]
  m
  setSGR [Reset]

-- | evaluate a Statement
evalStmt :: Stmt -> StateT (M.Map String Type) IO ()
evalStmt Quit = lift exitSuccess
evalStmt List = gets M.toList >>= \vars -> lift $ mapM_ list vars
evalStmt Reload = put richCtx
evalStmt Purge = put M.empty
evalStmt Help = lift help
evalStmt (Assignment var typ) = modify $ M.insert var typ
evalStmt (Check term) = do
  ctx <- get
  case evalTypeCheck term ctx of
      Left err -> do
        lift $ putStrLn ("Cannot infere type of " <> show term)
        lift $ colorize Red $ print err
      Right type' -> do
        lift $ putStr $ show term
        lift $ colorize Green $ putStr " : "
        lift $ print type'

list :: (String,Type) -> IO ()
list (var,typ) = putStr var >> colorize Green (putStr " : ") >> print typ

help :: IO ()
help = do
  putStrLn "Type a System F term to check its type."
  putStrLn "Or assign a type to a variable with ':'."
  putStrLn "Options:"
  putStrLn "  :q to exit"
  putStrLn "  :r to reload environment"
  putStrLn "  :p to purge environment"
  putStrLn "  :l to list all bindings in environment"
  putStrLn "  :h to display this information"
