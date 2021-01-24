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
   | UnBoundType Type

instance Show ErrorType where
  show (UnBoundVariable v) = v <> " is an unbound variable."
  show (UnBoundType t) = show t <> " is an unbound type variable."
  show (ArgMissmatch t1 t2) = "Expected argument of type "
                            <> show t1 <> " but found " <> show t2
  show (NoFuncInApplication t) = "Expected a function type in application but found "
                               <> show t

-- | Monad stack containing a Map as State and Exception handler
type Context a = ExceptT ErrorType (State (M.Map String Type)) a

-- | Typecheck a term using state as current scope
typeCheck :: Term -> Context Type
typeCheck (Variable v)
    = do
      ctx <- get
      case M.lookup v ctx of
        Nothing -> throwError $ UnBoundVariable v
        (Just typ) -> return typ
typeCheck (Abstraction v type1 t)
    = do
      modify $ M.insert v type1
      type2 <- typeCheck t
      return $ FunctionType type1 type2
typeCheck (Application t1 t2)
    = do
      ctx <- get
      type11 <- typeCheck t2
      put ctx
      type1 <- typeCheck t1
      case type1 of
        (FunctionType type11' type12)
                -> when (type11 /= type11')
                  (throwError $ ArgMissmatch type11' type11)
                  >> return type12
        t -> throwError $ NoFuncInApplication t
typeCheck (TypeAbstraction x t)
    = do
      modify (M.insert x $ TypeVariable x)
      type2 <- typeCheck t
      return $ UniversalType x type2
typeCheck (TypeApplication t (TypeVariable var))
    = do -- special case: look for var-def in ctx
      ctx <- get
      case M.lookup var ctx of
        (Just typ1) -> typeCheck t >>= \typ' ->
                       case typ' of
                         (UniversalType x t12) -> return $ subst typ1 x t12
                         _ -> throwError $ ArgMissmatch typ1 typ'
        Nothing -> throwError $ UnBoundType $ TypeVariable var
typeCheck (TypeApplication t typ)
    = typeCheck t >>= \typ' ->
        case typ' of
             (UniversalType x t12) -> return $ subst typ x t12
             _ -> throwError $ ArgMissmatch typ' typ

-- | Substitute t for s in a type
subst :: Type -> String -> Type -> Type
subst t s (TypeVariable str) | s == str  = t
                             | otherwise = TypeVariable str
subst t s (FunctionType t1 t2) = FunctionType (subst t s t1) (subst t s t2)
subst t s (UniversalType var t') | s == var  = UniversalType var t'
                                 | otherwise = UniversalType var $ subst t s t'
inType :: Type -> Type -> Bool
inType t (FunctionType t1 t2) = inType t t1 || inType t t2
inType t (UniversalType _ t') = inType t t'
inType t t'                   = t == t'

-- | Eval typechecker with empty context, no runtime exceptions
evalTypeCheck :: Term -> Either ErrorType Type
evalTypeCheck t = evalState (runExceptT $ typeCheck t) richCtx

-- | Default context, from exercise 2
defaultCtx :: M.Map String Type
defaultCtx = M.fromList [("const", UniversalType "A" (UniversalType "B" (FunctionType (TypeVariable "A") (FunctionType (TypeVariable "B") (TypeVariable "A"))))),
                         ("Nat", TypeVariable "Nat"), -- ?
                         ("five", TypeVariable "Nat")]

richCtx :: M.Map String Type
richCtx = M.fromList [ ("id", UniversalType "A" (FunctionType (TypeVariable "A") (TypeVariable "A")))
                     , ("Bool", bool)
                     , ("true", bool)
                     , ("false", bool)
                     , ("not", FunctionType bool bool)
                     , ("and", FunctionType bool (FunctionType bool bool))
                     , ("or", FunctionType bool (FunctionType bool bool))
                     , ("CNat", cnat)
                     , ("cone", cnat)
                     , ("ctwo", cnat)
                     , ("cthree", cnat)
                     , ("csucc", FunctionType cnat cnat)
                     , ("cplus", FunctionType cnat (FunctionType cnat cnat))
                     ] `M.union` defaultCtx
      where cnat = UniversalType "X" (FunctionType (FunctionType (TypeVariable "X") (TypeVariable "X"))
                                                   (FunctionType (TypeVariable "X") (TypeVariable "X")))
            bool = UniversalType "X" (FunctionType (TypeVariable "X") (FunctionType (TypeVariable "X") (TypeVariable "X")))

-------------------------------------------------------------------------------
-- Examples/Mess

-- | Run typechecker with empty context
runTypeCheck :: Term -> Type
runTypeCheck t = case evalState (runExceptT $ typeCheck t) defaultCtx of
                       Left err -> error $ "Can not infere type: " <> show err
                       Right typ -> typ


main :: IO ()
main = mapM_ (print . runTypeCheck) [term,term']

term = TypeAbstraction "A" (Abstraction "x" (TypeVariable "A") (Variable "x"))
term' = TypeAbstraction "C" (Application (TypeApplication (TypeApplication
     (Variable "const") (TypeVariable "Nat")) (TypeVariable "C")) (Variable "five"))
