{-# LANGUAGE GADTSyntax #-}

module Parsing where

import Data.Char (isDigit, isSpace)

data Token where
  TLit :: Integer -> Token
  TPlus :: Token
  TMinus :: Token
  TTimes :: Token
  LParen :: Token
  RParen :: Token
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x : xs)
  | isDigit x = TLit strDigit : tokenize rest
  | isSpace x = tokenize (dropWhile isSpace xs)
  | x == '+' = TPlus : tokenize xs
  | x == '-' = TMinus : tokenize xs
  | x == '*' = TTimes : tokenize xs
  | x == '(' = LParen : tokenize xs
  | x == ')' = RParen : tokenize xs
  where
    (digit, rest) = span isDigit xs
    strDigit = read $ x : digit :: Integer

precedence :: Token -> Int
precedence (TLit _) = 10
precedence TPlus = 4
precedence TMinus = 4
precedence TTimes = 5
precedence LParen = 0
precedence RParen = 0

shunt' :: [Token] -> [Token] -> [Token]
shunt' [] op = op
shunt' (l@(TLit _) : toks) op = l : shunt' toks op
shunt' (tok : toks) [] = shunt' toks [tok]
shunt' (LParen : toks) (op : ops) = shunt' toks (LParen : op : ops)
shunt' (RParen : toks) (op : ops) = if op == LParen then shunt' toks ops else op : shunt' (RParen : toks) ops
shunt' (tok : toks) (op : ops) =
  if precedence tok > precedence op
    then shunt' toks (tok : op : ops)
    else op : shunt' (tok : toks) ops

shunt :: [Token] -> [Token]
shunt input = shunt' input []

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  deriving (Show)

interp :: Arith -> Integer
interp (Lit n) = n
interp (Bin op a1 a2) = interpOp op (interp a1) (interp a2)

interpOp :: Op -> Integer -> Integer -> Integer
interpOp Plus = (+)
interpOp Minus = (-)
interpOp Times = (*)

getOp :: Token -> Op
getOp TMinus = Minus
getOp TPlus = Plus
getOp TTimes = Times

parsePostfix' :: [Token] -> [Arith] -> Arith
parsePostfix' [] (arith : rest) = arith
parsePostfix' ((TLit x) : toks) stk = parsePostfix' toks (Lit x : stk)
parsePostfix' (LParen : toks) stk = parsePostfix' toks stk
parsePostfix' (RParen : toks) stk = parsePostfix' toks stk
parsePostfix' (tok : toks) (y : x : stk) = parsePostfix' toks (Bin (getOp tok) x y : stk)

parsePostfix :: [Token] -> Arith
parsePostfix toks = parsePostfix' toks []

eval :: String -> Integer
eval = interp . parsePostfix . shunt . tokenize