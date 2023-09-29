{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Syntax
  ( Op
      ( Plus,
        Minus,
        Times,
        Exp
      ),
    Arith (Lit, Bin),
    expr1,
    expr2,
    expr3,
    expr4,
    expr5,
    expr6,
    expr7,
    prettyArith1,
    prettyPrec,
    Associativity (L, R),
    Precedence,
    expr8,
  )
where

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  Exp :: Op
  deriving (Eq)

instance Show Op where
  show :: Op -> String
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Exp = "^"

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  deriving (Show)

data Associativity where
  L :: Associativity
  R :: Associativity
  deriving (Show, Eq)

type Precedence = Int

assoc :: Op -> Associativity
assoc Plus = L
assoc Minus = L
assoc Times = L
assoc Exp = R

prec :: Op -> Precedence
prec Exp = 9
prec Times = 8
prec Plus = 4
prec Minus = 4

-- 4 * (5 + 2)
expr1 :: Arith
expr1 = Bin Times (Lit 4) (Bin Plus (Lit 5) (Lit 2))

-- 44 - 7 * (1 + 2) - 3
expr2 :: Arith
expr2 = Bin Minus (Lit 44) (Bin Minus (Bin Times (Lit 7) (Bin Plus (Lit 1) (Lit 2))) (Lit 3))

expr3 :: Arith
expr3 = Bin Plus (Bin Minus (Lit 7) (Lit 4)) (Lit 2)

expr4 :: Arith
expr4 = Bin Minus (Lit 7) (Bin Plus (Lit 4) (Lit 2))

expr5 :: Arith
expr5 = Bin Exp (Lit 2) (Bin Exp (Lit 3) (Lit 4))

-- (2+3) ^ 7 * 9
expr6 :: Arith
expr6 = Bin Exp (Bin Plus (Lit 2) (Lit 3)) (Bin Times (Lit 7) (Lit 9))

-- 5 + (2 * 3) == 5 + 2 * 3
expr7 :: Arith
expr7 = Bin Plus (Lit 5) (Bin Times (Lit 2) (Lit 3))

-- 4 * 5 * 2
expr8 :: Arith
expr8 = Bin Times (Lit 4) (Bin Times (Lit 5) (Lit 3))

prettyArith1 :: Arith -> String
prettyArith1 (Lit x) = show x
prettyArith1 (Bin op left right) = "(" ++ prettyArith1 left ++ show op ++ prettyArith1 right ++ ")"

prettyPrec :: Precedence -> Associativity -> Arith -> String
prettyPrec _ _ (Lit x) = show x
prettyPrec _ _ (Bin op l r) = prettyLeft p a l ++ show op ++ prettyRight p a r
  where
    p = prec op
    a = assoc op

prettyLeft :: Precedence -> Associativity -> Arith -> String
prettyLeft _ _ (Lit x) = show x
prettyLeft p _ (Bin op l r)
  | p <= prec op = prettyLeft (prec op) (assoc op) l ++ show op ++ prettyRight (prec op) (assoc op) r
  | otherwise = "(" ++ prettyLeft (prec op) (assoc op) l ++ show op ++ prettyRight (prec op) (assoc op) r ++ ")"

prettyRight :: Precedence -> Associativity -> Arith -> String
prettyRight _ _ (Lit x) = show x
prettyRight p L (Bin op l r)
  | p >= prec op = "(" ++ prettyLeft (prec op) (assoc op) l ++ show op ++ prettyRight (prec op) (assoc op) r ++ ")"
  | otherwise = prettyLeft (prec op) (assoc op) l ++ show op ++ prettyRight (prec op) (assoc op) r
prettyRight p R (Bin op l r)
  | p > prec op = "(" ++ prettyLeft (prec op) (assoc op) l ++ show op ++ prettyRight (prec op) (assoc op) r ++ ")"
  | otherwise = prettyLeft (prec op) (assoc op) l ++ show op ++ prettyRight (prec op) (assoc op) r
