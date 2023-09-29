module Compiler (compile, exec, eval) where

import AbstractMachine (Instruction (ADD, MUL, PUSH, SUB), Program, run)
import Interpreter (interp)
import Parsing
import Syntax (Arith (Bin, Lit), Op (Exp, Minus, Plus, Times))
import Prelude hiding ((*>), (<$), (<$>), (<*), (<*>))

readArith :: String -> Arith
readArith s = case parse parseArith s of
  Left err -> error (show err)
  Right a -> a

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

integer :: Parser Integer
integer = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseAtom :: Parser Arith
parseAtom = Lit <$> integer <|> parens parseExpr

parseExpr :: Parser Arith
parseExpr = buildExpressionParser table parseAtom
  where
    -- Each list of operators in the table has the same precedence, and
    -- the lists are ordered from highest precedence to lowest.  So
    -- in this case '*' has the highest precedence, and then "+" and
    -- "-" have lower (but equal) precedence.
    table =
      [ [binary "*" (Bin Times) AssocLeft],
        [ binary "+" (Bin Plus) AssocLeft,
          binary "-" (Bin Minus) AssocLeft
        ]
      ]

    binary name fun = Infix (reservedOp name >> return fun)

parseArith :: Parser Arith
parseArith = whiteSpace *> parseExpr <* eof

eval :: String -> Maybe Integer
eval s = case parse parseArith s of
  Left _ -> Nothing
  Right a -> Just (interp a)

compile :: Arith -> Program
compile (Lit x) = [PUSH x]
compile (Bin Plus l r) = compile l ++ compile r ++ [ADD]
compile (Bin Times l r) = compile l ++ compile r ++ [MUL]
compile (Bin Minus l r) = compile l ++ compile r ++ [SUB]
compile (Bin Exp l r) = compile l ++ compile r ++ [MUL]

exec :: String -> Maybe Integer
exec = run . compile . readArith