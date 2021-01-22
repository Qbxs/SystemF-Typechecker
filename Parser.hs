module Parser (parseTerm) where

import Text.ParserCombinators.Parsec
import TypeChecker (Term(..), Type(..))


parseTerm :: String -> Either ParseError Term
parseTerm = parse term "ParseError:"

keyword :: String -> Parser String
keyword k = try $ string k

term :: Parser Term
term =  try abstraction
    <|> try tAbstraction
    <|> try application
    <|> try tApplication
    <|> variable

lambda :: Parser String
lambda = keyword "\\" <|> keyword "λ"

variable :: Parser Term
variable = do
  str <- many1 lower
  return $ Variable str

abstraction :: Parser Term
abstraction = do
  lambda
  str <- many1 lower
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
  str <- many1 upper
  char '.'
  spaces
  t <- term
  return $ TypeAbstraction str t

tApplication :: Parser Term
tApplication = do
  keyword "("
  t <- term
  space
  spaces
  keyword "["
  type' <- typ
  keyword "]"
  keyword ")"
  return $ TypeApplication t type'


typ :: Parser Type
typ =  tUniversal
   <|> tFunc
   <|> tVar

forall :: Parser String
forall = keyword "forall" <|> keyword "∀"

tVar :: Parser Type
tVar = do
  str <- many1 upper
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
  forall
  spaces
  str <- many1 upper
  spaces
  keyword "."
  spaces
  type' <- typ
  return $ UniversalType str type'
