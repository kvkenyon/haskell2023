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

ppS :: Parser String
ppS =
  whiteSpace lexer
    >> char '('
    >> whiteSpace lexer
    >> parseString
    >>= (\p -> whiteSpace lexer >> return p)
    >>= (\p -> char ')' >> return p)
    >>= (\p -> whiteSpace lexer >> return p)
    >>= (\p -> return ("(" ++ p ++ ")"))

term =
  parseParen
    <|> parseArithAtom

binary name fun = Infix (do operator name; return fun)

prefix name fun = Prefix (do operator name; return fun)

table =
  [ [prefix "-" (Unary Neg)],
    [binary "^" (Bin Exp) AssocRight],
    [binary "*" (Bin Times) AssocLeft, binary "/" (Bin Div) AssocLeft],
    [binary "+" (Bin Plus) AssocLeft, binary "-" (Bin Minus) AssocLeft]
  ]

parseArith :: Parser ArithE
parseArith = buildExpressionParser table term

strTerm = parseParenS <|> showDouble <$> number
  where
    showDouble n
      | Left i <- n = show (fromIntegral i :: Double)
      | Right d <- n = show d
    hasParens = string "(" >> parseString

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