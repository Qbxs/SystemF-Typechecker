{-# LANGUAGE LambdaCase #-}

module Main where

import System.Console.ANSI
import System.IO
import Control.Monad.State
import qualified Data.Map as M
import System.Exit (exitSuccess)
import TypeChecker hiding (term, main)
import Parser


main :: IO ()
main = setTitle "SystemF - Repl" >> evalStateT loop richCtx

-- | REPL with variable context
loop :: StateT (M.Map String Type) IO ()
loop = do
  lift $ colorize Blue $ putStr "λF> "
  lift $ hFlush stdout
  str <- lift getLine
  case parseStmt str of
    Left err -> lift (print err) >> loop
    Right stmt -> evalStmt stmt >> loop


-- | evaluate a Statement
evalStmt :: Stmt -> StateT (M.Map String Type) IO ()
evalStmt Quit = lift $ putStrLn "Bye bye" >> exitSuccess
evalStmt List = gets M.toList >>= \vars -> lift $ mapM_ list vars
evalStmt Reload = put richCtx
evalStmt Purge = put M.empty
evalStmt Help = lift help
evalStmt (Assignment var typ) = modify $ M.insert var typ
evalStmt (Check term) = gets (evalTypeCheck term)
  >>= \case
      Left err -> lift $ do
        putStrLn ("Cannot infere type of " <> show term)
        colorize Red $ print err
      Right type' -> lift $ do
        putStr $ show term
        colorize Green $ putStr " : "
        print type'

-- | Set and reset color for a given IO-action
colorize :: Color -> IO () -> IO ()
colorize c m = setSGR [SetColor Foreground Vivid c] >> m >> setSGR [Reset]

-- | Helper function to print context
list :: (String,Type) -> IO ()
list (var,typ) = putStr var >> colorize Green (putStr " : ") >> print typ

help :: IO ()
help = mapM_ putStrLn
  [ "Type a System F term to check its type."
  , "Or assign a type to a variable with ':'."
  , "Options:"
  , "  :q to exit"
  , "  :r to reload environment"
  , "  :p to purge environment"
  , "  :l to list all bindings in environment"
  , "  :h to display this information"
  ]
