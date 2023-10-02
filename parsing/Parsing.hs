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
shunt' (tok : toks) (op : ops) =
  if precedence tok > precedence op
    then shunt' toks (tok : op : ops)
    else op : shunt' (tok : toks) ops

shunt :: [Token] -> [Token]
shunt input = shunt' input []
