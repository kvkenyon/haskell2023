module Parser where

import Parsing
import Syntax

lexer :: TokenParser u
lexer =
  makeTokenParser $
    emptyDef
      { reservedNames =
          [ "True",
            "False",
            "if",
            "then",
            "else",
            "begin",
            "end",
            "repeat",
            "while",
            "input",
            "output",
            "int",
            "bool"
          ],
        reservedOpNames = [":=", "==", "<", "+", "-", "*", "!", "&&", "||"]
      }

parens :: Parser a -> Parser a
parens = getParens lexer

reserved, reservedOp :: String -> Parser ()
reserved = getReserved lexer
reservedOp = getReservedOp lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

ident :: Parser String
ident = getIdentifier lexer

integer :: Parser Integer
integer = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseAtom :: Parser Expr
parseAtom =
  EInt
    <$> integer
      <|> EBool True
    <$ reserved "True"
      <|> EBool False
    <$ reserved "False"
      <|> EVar
    <$> ident
      <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table =
      [ [unary "!" (EUn Not)],
        [unary "-" (EUn Neg)],
        [ binary "*" (EBin Mul) AssocLeft,
          binary "/" (EBin Div) AssocLeft
        ],
        [ binary "+" (EBin Add) AssocLeft,
          binary "-" (EBin Sub) AssocLeft
        ],
        [ binary "==" (EBin Equals) AssocNone,
          binary "<" (EBin Less) AssocNone
        ],
        [binary "&&" (EBin And) AssocRight],
        [binary "||" (EBin Or) AssocRight]
      ]
    unary name fun = Prefix (fun <$ reservedOp name)
    binary name fun assoc = Infix (fun <$ reservedOp name) assoc

parseProg :: Parser Prog
parseProg = parseStmt `sepBy` (reservedOp ";")

parseStmt :: Parser Stmt
parseStmt =
  parseBlock
    <|> If
    <$> (reserved "if" *> parseExpr)
    <*> (reserved "then" *> parseStmt)
    <*> (reserved "else" *> parseStmt)
      <|> Repeat
    <$> (reserved "repeat" *> parseExpr)
    <*> parseBlock
      <|> While
    <$> (reserved "while" *> parseExpr)
    <*> parseBlock
      <|> Input
    <$> (reserved "input" *> ident)
      <|> Output
    <$> (reserved "output" *> parseExpr)
      <|> Assign
    <$> ident
    <*> (reservedOp ":=" *> parseExpr)
      <|> Decl
    <$> parseType
    <*> ident

parseType :: Parser Type
parseType = (TyInt <$ reserved "int") <|> (TyBool <$ reserved "bool")

parseBlock :: Parser Stmt
parseBlock = Block <$> (symbol "{" *> parseProg <* symbol "}")

impParser :: Parser Prog
impParser = whiteSpace *> parseProg <* eof
