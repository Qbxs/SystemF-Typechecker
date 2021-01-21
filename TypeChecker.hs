module TypeChecker where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

data Term =
  Variable String |
  Abstraction String Type Term |
  Application Term Term |
  TypeAbstraction String Term |
  TypeApplication Term Type
    deriving (Eq, Ord)

instance Show Term where
  show (Variable var) = var
  show (Abstraction var typ t) = "λ" <> var <> ":" <> show typ <> "." <> show t
  show (Application t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
  show (TypeAbstraction var t) = "λ" <> var <> "." <> show t
  show (TypeApplication t1 t2) = "(" <> show t1 <> " [" <> show t2 <> "])"


data Type =
  TypeVariable String |
  FunctionType Type Type |
  UniversalType String Type
    deriving (Eq, Ord)

instance Show Type where
  show (TypeVariable var) = var
  show (FunctionType t1 t2) = "(" <> show t1 <> " → " <> show t2 <> ")"
  show (UniversalType var t) = "∀" <> var <> "." <> show t

data ErrorType =
  UnBoundVariable String |
  ArgMissmatch Type Type|
  NoFuncInApplication Type

instance Show ErrorType where
  show (UnBoundVariable v) = v <> " is an unbound variable."
  show (ArgMissmatch t1 t2) = "Argument missmatch: Expected argument of type "
                            <> show t1 <> " but found " <> show t2
  show (NoFuncInApplication t) = "Expected a function type in application but found "
                               <> show t

type Context a = ExceptT ErrorType (State (M.Map String Type)) a

typeCheck :: Term -> Context Type
typeCheck (Variable v) = get >>= \ctx -> case M.lookup v ctx of
                          Nothing -> throwError $ UnBoundVariable v
                          (Just typ) -> return typ
typeCheck (Abstraction v type1 t) = modify (M.insert v type1)
                                  >> typeCheck t
                                  >>= \type2 -> return $ FunctionType type1 type2
typeCheck (Application t1 t2) = do
  s <- get
  type11 <- typeCheck t2
  put s -- reset state (scope)
  type1 <- typeCheck t1
  case type1 of
    (FunctionType type11' type12) -> when (type11 /= type11')
                                          (throwError $ ArgMissmatch type11 type11')
                                  >> return type12
    t -> throwError $ NoFuncInApplication t

typeCheck (TypeAbstraction x t) = modify (M.insert x $ TypeVariable x)
                                >> typeCheck t
                                >>= \type2 -> return $ UniversalType x type2
typeCheck (TypeApplication t typ) = typeCheck t
                                  >>= \(UniversalType x t12) -> return $ subst typ x t12
                  where
                    subst :: Type -> String -> Type -> Type
                    subst t s (TypeVariable str) | s == str  = TypeVariable s
                                                 | otherwise = TypeVariable str
                    subst t s (FunctionType t1 t2) = FunctionType (subst t s t1) (subst t s t2)
                    subst _ _ u@(UniversalType _ _) = u

runTypeCheck :: Term -> Type
runTypeCheck t = case evalState (runExceptT $ typeCheck t) M.empty of
                       Left err -> error $ "Can not infere type: " <> show err
                       Right typ -> typ

evalTypeCheck :: Term -> Either ErrorType Type
evalTypeCheck t = evalState (runExceptT $ typeCheck t) M.empty

main :: IO ()
main = mapM_ (print . runTypeCheck) [term]

term = TypeAbstraction "A" (Abstraction "x" (TypeVariable "A") (Variable "x"))
