{-# LANGUAGE BlockArguments #-}

module Parser where

import Control.Applicative (liftA3)
import Control.Monad (liftM)
import Parsing2
import Syntax
import qualified Text.Parsec as P
import Text.Parsec.Token (GenTokenParser (whiteSpace))

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

number :: Parser (Either Integer Double)
number = getNaturalOrFloat lexer

operator :: String -> Parser ()
operator = getReservedOp lexer

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
  return (Func f LParen p RParen)

parseFuncs :: Parser ArithE
parseFuncs =
  parseFunc Sin
    <|> parseFunc Cos
    <|> parseFunc Tan
    <|> parseFunc Log
    <|> parseFunc Sqrt
    <|> parseFunc Round

binary name fun = Infix (do operator name; return fun)

prefix name fun = Prefix (do operator name; return fun)

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

parseConstantS :: C -> Parser String
parseConstantS c = do
  getSymbol lexer (constSymbol c)
  return (show c)

parseConstantsS :: Parser String
parseConstantsS = parseConstantS E <|> parseConstantS Pi

parseParenS :: Parser String
parseParenS = do
  whiteSpace lexer
  char '('
  whiteSpace lexer
  p <- parseString
  whiteSpace lexer
  char ')'
  whiteSpace lexer
  return ("(" ++ p ++ ")")

parseFuncS :: F -> Parser String
parseFuncS f = do
  whiteSpace lexer
  s <- getSymbol lexer $ show f
  whiteSpace lexer
  char '('
  whiteSpace lexer
  p <- parseString <|> parseFuncsS
  whiteSpace lexer
  char ')'
  whiteSpace lexer
  return (show f ++ "(" ++ p ++ ")")

parseFuncsS :: Parser String
parseFuncsS =
  parseFuncS Sin
    <|> parseFuncS Cos
    <|> parseFuncS Tan
    <|> parseFuncS Log
    <|> parseFuncS Sqrt
    <|> parseFuncS Round

strTerm = parseParenS <|> parseFuncsS <|> parseConstantsS <|> showDouble <$> number
  where
    showDouble n
      | Left i <- n = show (fromIntegral i :: Double)
      | Right d <- n = show d

parseString :: Parser String
parseString =
  buildExpressionParser
    [ [prefix "-" (" -" ++)],
      [binary "^" (\l r -> l ++ " ^ " ++ r) AssocRight],
      [binary "*" (\l r -> l ++ " * " ++ r) AssocLeft, binary "/" (\l r -> l ++ " / " ++ r) AssocLeft],
      [binary "+" (\l r -> l ++ " + " ++ r) AssocLeft, binary "-" (\l r -> l ++ " - " ++ r) AssocLeft]
    ]
    strTerm

pretty :: String -> Either P.ParseError String
pretty = parse $ getWhiteSpace lexer *> parseString <* eof

arith :: String -> Either P.ParseError ArithE
arith = parse $ getWhiteSpace lexer *> parseArith <* eof