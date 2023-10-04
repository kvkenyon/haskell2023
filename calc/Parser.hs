{-# LANGUAGE BlockArguments #-}

module Parser where

import Parsing2
import Syntax
import qualified Text.Parsec as P

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

number :: Parser (Either Integer Double)
number = getNaturalOrFloat lexer

operator :: String -> Parser ()
operator = getReservedOp lexer

parseArithAtom :: Parser Arith
parseArithAtom = Lit <$> getNaturalOrFloat lexer

term =
  parens parseArith
    <|> parseArithAtom

binary name fun = Infix (do operator name; return fun)

prefix name fun = Prefix (do operator name; return fun)

table =
  [ [prefix "-" Neg],
    [binary "^" Exp AssocRight],
    [binary "*" Mult AssocLeft, binary "/" Div AssocLeft],
    [binary "+" Add AssocLeft, binary "-" Subt AssocLeft]
  ]

parseArith :: Parser Arith
parseArith = buildExpressionParser table term

strTerm = parens parseString <|> showDouble <$> number
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

arith :: String -> Either P.ParseError Arith
arith = parse $ getWhiteSpace lexer *> parseArith <* eof