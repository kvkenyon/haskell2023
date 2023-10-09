module Parser where

import Parsing
import Syntax

lexer :: TokenParser u
lexer =
  makeTokenParser $
    emptyDef
      { reservedNames = ["let", "in", "if", "then", "else", "False", "True"]
      }

parens :: Parser a -> Parser a
parens = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

integer :: Parser Integer
integer = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

identifier :: Parser String
identifier = getIdentifier lexer

parseVar :: Parser Arith
parseVar = do
  whiteSpace
  varName <- identifier
  whiteSpace
  return $ Var varName

parseLet :: Parser Arith
parseLet = do
  whiteSpace
  reserved "let"
  (Var x) <- parseVar
  char '=' >> whiteSpace
  arith1 <- parseArith
  whiteSpace
  reserved "in"
  whiteSpace
  Let x arith1 <$> parseArith

parseIf :: Parser Arith
parseIf = do
  whiteSpace
  reserved "if"
  whiteSpace
  a1 <- parseArith
  whiteSpace
  reserved "then"
  whiteSpace
  a2 <- parseArith
  whiteSpace
  reserved "else"
  whiteSpace
  If a1 a2 <$> parseArith

parseBool :: Parser Arith
parseBool = Lit . Boolean <$> ((reserved "True" >> return True) <|> (reserved "False" >> return False))

parseArithAtom :: Parser Arith
parseArithAtom =
  (Lit . Number <$> integer)
    <|> parseBool
    <|> parseVar
    <|> parseLet
    <|> parseIf
    <|> parens parseArith

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table =
      [ [Infix (Bin Times <$ reservedOp "*") AssocLeft, Infix (Bin Div <$ reservedOp "/") AssocLeft],
        [ Infix (Bin Plus <$ reservedOp "+") AssocLeft,
          Infix (Bin Minus <$ reservedOp "-") AssocLeft
        ],
        [Infix (Bin Less <$ reservedOp "<") AssocNone, Infix (Bin Equal <$ reservedOp "==") AssocNone]
      ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof
