{-# LANGUAGE GADTSyntax #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import qualified Data.Map as M
import Parsing

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  Var :: String -> Arith
  Let :: Arith -> Arith -> Arith -> Arith
  deriving (Show)

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  Div :: Op
  deriving (Show, Eq)

type Env = M.Map String Integer

data InterpError = UndefinedVar String | DivByZero

interpArith :: Env -> Arith -> Either InterpError Integer
interpArith _ (Lit i) = Right i
interpArith e (Var x) = case M.lookup x e of
  Just i -> Right i
  Nothing -> Left $ UndefinedVar x
interpArith e (Let (Var v) a1 a2) = interpArith e a1 >>= (\x -> interpArith (M.insert v x e) a2)
interpArith e (Bin Plus e1 e2) = (+) <$> interpArith e e1 <*> interpArith e e2
interpArith e (Bin Minus e1 e2) = (-) <$> interpArith e e1 <*> interpArith e e2
interpArith e (Bin Times e1 e2) = (*) <$> interpArith e e1 <*> interpArith e e2
interpArith e (Bin Div e1 e2) = do
  numerator <- interpArith e e1
  denom <- interpArith e e2
  case denom of
    0 -> Left DivByZero
    _ -> Right $ div numerator denom

showInterpError :: InterpError -> String
showInterpError (UndefinedVar x) = "Runtime Error: Undefined variable " ++ x
showInterpError DivByZero = "Runtime Error: Division by zero"

lexer :: TokenParser u
lexer =
  makeTokenParser $
    emptyDef
      { reservedNames = ["let", "in"]
      }

-- tell the lexer that "let" and "in" are reserved keywords
-- which may not be used as variable names

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
  var <- parseVar
  char '=' >> whiteSpace
  arith1 <- parseArith
  whiteSpace
  reserved "in"
  whiteSpace
  Let var arith1 <$> parseArith

parseArithAtom :: Parser Arith
parseArithAtom = (Lit <$> integer) <|> parseVar <|> parseLet <|> parens parseArith

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table =
      [ [Infix (Bin Times <$ reservedOp "*") AssocLeft, Infix (Bin Div <$ reservedOp "/") AssocLeft],
        [ Infix (Bin Plus <$ reservedOp "+") AssocLeft,
          Infix (Bin Minus <$ reservedOp "-") AssocLeft
        ]
      ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof

eval :: String -> IO ()
eval s = do
  case parse arith s of
    Left parseError -> print parseError
    Right ast -> case interpArith M.empty ast of
      Left interpreterError -> print $ showInterpError interpreterError
      Right val -> print val

main :: IO ()
main = do
  eval "let x = 1 + 2 in x * x"