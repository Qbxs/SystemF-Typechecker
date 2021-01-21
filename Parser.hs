module Parser (parseTerm) where

import Text.ParserCombinators.Parsec
import TypeChecker hiding (term)

keyword :: String -> Parser String
keyword k = try $ string k

term :: Parser Term
term = abstraction
   <|> tAbstraction
   <|> application
   <|> tApplication
   <|> variable

lambda :: Parser String
lambda = keyword "\\" <|> keyword "λ"

variable :: Parser Term
variable = do
  str <- many lower
  return $ Variable str

abstraction :: Parser Term
abstraction = do
  lambda
  str <- many lower
  spaces
  char ':'
  spaces
  type' <- typ
  char '.'
  spaces
  t <- term
  return $ Abstraction str type' t

application :: Parser Term
application = do
  keyword "("
  t1 <- term
  space
  spaces
  t2 <- term
  keyword ")"
  return $ Application t1 t2

tAbstraction :: Parser Term
tAbstraction = do
  lambda
  str <- many upper
  char '.'
  spaces
  t <- term
  return $ TypeAbstraction (read str) t

tApplication :: Parser Term
tApplication = do
  keyword "("
  t <- term
  space
  spaces
  type' <- typ
  keyword ")"
  return $ TypeApplication t type'


typ :: Parser Type
typ = tFunc
  <|> tUniversal
  <|> tVar

tVar :: Parser Type
tVar = do
  str <- many upper
  return $ TypeVariable str

tFunc :: Parser Type
tFunc = do
  keyword "("
  type1 <- typ
  spaces
  keyword "->"
  spaces
  type2 <- typ
  spaces
  keyword ")"
  return $ FunctionType type1 type2

tUniversal :: Parser Type
tUniversal = do
  keyword "forall"
  spaces
  str <- many upper
  spaces
  keyword "."
  spaces
  type' <- typ
  return $ UniversalType (read str :: String) type'

parseTerm :: String -> Either ParseError Term
parseTerm input = parse term "ParseError:" input
