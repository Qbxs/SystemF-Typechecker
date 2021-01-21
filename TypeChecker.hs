{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

data Term
   = Variable String
   | Abstraction String Type Term
   | Application Term Term
   | TypeAbstraction String Term
   | TypeApplication Term Type
  deriving (Eq, Ord)

instance Show Term where
  show (Variable var) = var
  show (Abstraction var typ t) = "λ" <> var <> ":" <> show typ <> "." <> show t
  show (Application t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
  show (TypeAbstraction var t) = "λ" <> var <> "." <> show t
  show (TypeApplication t1 t2) = "(" <> show t1 <> " [" <> show t2 <> "])"


data Type
   = TypeVariable String
   | FunctionType Type Type
   | UniversalType String Type
  deriving (Eq, Ord)

instance Show Type where
  show (TypeVariable var) = var
  show (FunctionType t1 t2) = "(" <> show t1 <> " → " <> show t2 <> ")"
  show (UniversalType var t) = "∀" <> var <> "." <> show t


data ErrorType
   = UnBoundVariable String
   | ArgMissmatch Type Type
   | NoFuncInApplication Type

instance Show ErrorType where
  show (UnBoundVariable v) = v <> " is an unbound variable."
  show (ArgMissmatch t1 t2) = "Expected argument of type "
                            <> show t1 <> " but found " <> show t2
  show (NoFuncInApplication t) = "Expected a function type in application but found "
                               <> show t

-- | Monad stack containing a Map as State and Exception handler
type Context a = ExceptT ErrorType (State (M.Map String Type)) a

-- | Typecheck a term using state as current scope
typeCheck :: Term -> Context Type
typeCheck (Variable v) = get >>= \ctx -> case M.lookup v ctx of
                          Nothing -> throwError $ UnBoundVariable v
                          (Just typ) -> return typ
typeCheck (Abstraction v type1 t) = modify (M.insert v type1)
                                  >> typeCheck t
                                  >>= \type2 -> return $ FunctionType type1 type2
typeCheck (Application t1 t2) = get
                              >>= \s -> typeCheck t2
                              >>= \type11 -> put s
                              >> typeCheck t1
                              >>= \case
                                (FunctionType type11' type12)
                                  -> when (type11 /= type11')
                                    (throwError $ ArgMissmatch type11' type11)
                                    >> return type12
                                t -> throwError $ NoFuncInApplication t
  -- type11 <- typeCheck t2
  -- put s -- reset state (scope)
  -- type1 <- typeCheck t1
  -- case type1 of
  --   (FunctionType type11' type12) -> when (type11 /= type11')
  --                                         (throwError $ ArgMissmatch type11 type11')
  --                                 >> return type12
  --   t -> throwError $ NoFuncInApplication t
typeCheck (TypeAbstraction x t) = modify (M.insert x $ TypeVariable x)
                                >> typeCheck t
                                >>= \type2 -> return $ UniversalType x type2
typeCheck (TypeApplication t typ) = typeCheck t
                                  >>= \t' -> case t' of
                                    (UniversalType x t12) -> return $ subst typ x t12
                                    _ -> throwError $ ArgMissmatch typ t'
                  where -- | Substitute t for s in a type
                    subst :: Type -> String -> Type -> Type
                    subst t s (TypeVariable str) | s == str  = t
                                                 | otherwise = TypeVariable str
                    subst t s (FunctionType t1 t2) = FunctionType (subst t s t1) (subst t s t2)
                    subst _ _ u@(UniversalType _ _) = u

-- | Run typechecker with empty context
runTypeCheck :: Term -> Type
runTypeCheck t = case evalState (runExceptT $ typeCheck t) M.empty of
                       Left err -> error $ "Can not infere type: " <> show err
                       Right typ -> typ

-- | Eval typechecker with empty context, no runtime exceptions
evalTypeCheck :: Term -> Either ErrorType Type
evalTypeCheck t = evalState (runExceptT $ typeCheck t) M.empty


-------------------------------------------------------------------------------
-- Examples/Mess

main :: IO ()
main = mapM_ (print . runTypeCheck) [term]

term = TypeAbstraction "A" (Abstraction "x" (TypeVariable "A") (Variable "x"))

runTypeCheckex2 :: Type
runTypeCheckex2 = case evalState (runExceptT $ typeCheck t) ctx of
                       Left err -> error $ "Can not infere type: " <> show err
                       Right typ -> typ
            where t = TypeAbstraction "C" (Application (TypeApplication (TypeApplication (Variable "const") (TypeVariable "N")) (TypeVariable "C")) (Variable "five"))
                  ctx = M.fromList [("const", UniversalType "A" (UniversalType "B" (FunctionType (TypeVariable "A") (FunctionType (TypeVariable "B") (TypeVariable "A"))))),
                                    ("N", TypeVariable "N"), -- ?
                                    ("five", TypeVariable "N")]
