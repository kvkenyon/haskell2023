module ArithParser where

import Parser (binary, lexer, number, prefix)
import Parsing2
  ( Assoc (AssocLeft, AssocRight),
    Parser,
    buildExpressionParser,
    char,
    eof,
    getNaturalOrFloat,
    getSymbol,
    getWhiteSpace,
    parse,
    (<|>),
  )
import Syntax (ArithE (..), C (..), F (..), Op (..), Paren (..), constSymbol)
import qualified Text.Parsec as P
import Text.Parsec.Token (GenTokenParser (whiteSpace))

parseArithAtom :: Parser ArithE
parseArithAtom = LitE <$> getNaturalOrFloat lexer

parseConstant :: C -> Parser ArithE
parseConstant c = do
  getSymbol lexer (constSymbol c)
  return (Const c)

parseConstants :: Parser ArithE
parseConstants = parseConstant E <|> parseConstant Pi

parseParen :: Parser ArithE
parseParen = do
  whiteSpace lexer
  char '('
  whiteSpace lexer
  p <- parseArith
  whiteSpace lexer
  char ')'
  whiteSpace lexer
  return (Par LParen p RParen)

parseFunc :: F -> Parser ArithE
parseFunc f = do
  whiteSpace lexer
  s <- getSymbol lexer $ show f
  whiteSpace lexer
  char '('
  whiteSpace lexer
  p <- parseArith <|> parseFuncs
  whiteSpace lexer
  char ')'
  whiteSpace lexer
  return (Func f LParen p LParen)

parseFuncs :: Parser ArithE
parseFuncs =
  parseFunc Sin
    <|> parseFunc Cos
    <|> parseFunc Tan
    <|> parseFunc Log
    <|> parseFunc Sqrt
    <|> parseFunc Round

term =
  parseParen
    <|> parseFuncs
    <|> parseConstants
    <|> parseArithAtom

table =
  [ [prefix "-" (Unary Neg)],
    [binary "^" (Bin Exp) AssocRight],
    [binary "*" (Bin Times) AssocLeft, binary "/" (Bin Div) AssocLeft],
    [binary "+" (Bin Plus) AssocLeft, binary "-" (Bin Minus) AssocLeft]
  ]

parseArith :: Parser ArithE
parseArith = buildExpressionParser table term

arith :: String -> Either P.ParseError ArithE
arith = parse $ getWhiteSpace lexer *> parseArith <* eof