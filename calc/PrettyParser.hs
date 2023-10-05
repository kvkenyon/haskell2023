module PrettyParser where

import Parser (binary, lexer, number, prefix)
import Parsing2
  ( Assoc (AssocLeft, AssocRight),
    Parser,
    buildExpressionParser,
    char,
    eof,
    getSymbol,
    getWhiteSpace,
    parse,
    (<|>),
  )
import Syntax (C (..), F (..), constSymbol)
import qualified Text.Parsec as P
import Text.Parsec.Token (GenTokenParser (whiteSpace))

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
