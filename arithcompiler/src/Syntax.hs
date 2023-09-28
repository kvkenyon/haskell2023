{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Syntax
  ( Op
      ( Plus,
        Minus,
        Times
      ),
    Arith (Lit, Bin),
    expr1,
    expr2,
    prettyArith1,
    prettyPrec,
    Associativity (L, R),
    Precedence,
  )
where

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  deriving (Eq)

instance Show Op where
  show :: Op -> String
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

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

prec :: Op -> Precedence
prec Times = 8
prec Plus = 4
prec Minus = 4

-- 4 * (5 + 2)
expr1 :: Arith
expr1 = Bin Times (Lit 4) (Bin Plus (Lit 5) (Lit 2))

-- 44 - 7 * (1 + 2) - 3
expr2 :: Arith
expr2 = Bin Minus (Lit 44) (Bin Minus (Bin Times (Lit 7) (Bin Plus (Lit 1) (Lit 2))) (Lit 3))

prettyArith1 :: Arith -> String
prettyArith1 (Lit x) = show x
prettyArith1 (Bin op left right) = "(" ++ prettyArith1 left ++ show op ++ prettyArith1 right ++ ")"

prettyPrec :: Precedence -> Associativity -> Arith -> String
prettyPrec _ _ (Lit x) = show x
prettyPrec _ _ (Bin op (Lit x) (Lit y)) = show x ++ show op ++ show y
prettyPrec p L (Bin op l@(Bin op' _ _) r@(Lit _)) =
  if p > prec op'
    then "(" ++ prettyPrec (prec op') (assoc op') l ++ ")" ++ show op ++ prettyPrec p L r
    else prettyPrec (prec op') (assoc op') l ++ show op ++ prettyPrec p L r
prettyPrec p R (Bin op l@(Bin op' _ _) r@(Lit _)) = prettyPrec (prec op') (assoc op') l ++ show op ++ prettyPrec p R r
prettyPrec p L (Bin op l@(Lit _) r@(Bin op' _ _)) = prettyPrec p L l ++ show op ++ prettyPrec (prec op') (assoc op') r
prettyPrec p R (Bin op l@(Lit _) r@(Bin op' _ _)) =
  if p > prec op'
    then prettyPrec p R l ++ show op ++ "(" ++ prettyPrec (prec op') (assoc op') r ++ ")"
    else prettyPrec p R l ++ show op ++ prettyPrec (prec op') (assoc op') r
prettyPrec p L (Bin rootOp l@(Bin op _ _) r@(Bin op' _ _)) =
  if p > prec op
    then
      ( if p > prec op'
          then
            "("
              ++ prettyPrec (prec op) (assoc op) l
              ++ ")"
              ++ show rootOp
              ++ "("
              ++ prettyPrec (prec op') (assoc op') r
              ++ ")"
          else "(" ++ prettyPrec (prec op) (assoc op) l ++ ")" ++ show rootOp ++ prettyPrec (prec op') (assoc op') r
      )
    else prettyPrec (prec op) (assoc op) l ++ show op ++ prettyPrec (prec op') (assoc op') r
prettyPrec p R (Bin rootOp l@(Bin op _ _) r@(Bin op' _ _)) =
  if p > prec op'
    then
      prettyPrec (prec op) (assoc op) l
        ++ show rootOp
        ++ "("
        ++ prettyPrec (prec op') (assoc op') r
        ++ ")"
    else prettyPrec (prec op) (assoc op) l ++ show rootOp ++ prettyPrec (prec op') (assoc op') r
