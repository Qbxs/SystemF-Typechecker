module Parser (parseTerm) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char (alphaNum)
import TypeChecker (Term(..), Type(..))


parseTerm :: String -> Either ParseError Term
parseTerm = parse term "ParseError:"


forall :: Parser String
forall = string "forall" <|> string "∀"

lambda :: Parser Char
lambda = char '\\' <|> char '\955'

firstUpper :: Parser String
firstUpper = upper >>= \c -> many alphaNum >>= \str -> return $ c:str

firstLower :: Parser String
firstLower = lower >>= \c -> many alphaNum >>= \str -> return $ c:str


term :: Parser Term
term = choice [ abstraction
              , tAbstraction
              , application
              , tApplication
              , variable
              ]

variable :: Parser Term
variable = Variable <$> firstLower

abstraction :: Parser Term
abstraction = do
  lambda
  str <- firstLower
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
  char '('
  t1 <- term
  space
  spaces
  t2 <- term
  char ')'
  return $ Application t1 t2

tAbstraction :: Parser Term
tAbstraction = do
  lambda
  str <- firstUpper
  char '.'
  spaces
  t <- term
  return $ TypeAbstraction str t

tApplication :: Parser Term
tApplication = do
  char '('
  t <- term
  space
  spaces
  optional $ char '['
  type' <- typ
  optional $ char ']'
  char ')'
  return $ TypeApplication t type'


typ :: Parser Type
typ = choice [ tUniversal
             , tFunc
             , tVar
             ]

tVar :: Parser Type
tVar = TypeVariable <$> firstUpper

tFunc :: Parser Type
tFunc = do
  char '('
  type1 <- typ
  spaces
  string "->"
  spaces
  type2 <- typ
  spaces
  char ')'
  return $ FunctionType type1 type2

tUniversal :: Parser Type
tUniversal = do
  forall
  spaces
  str <- firstUpper
  spaces
  char '.'
  spaces
  type' <- typ
  return $ UniversalType str type'
