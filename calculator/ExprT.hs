{-# LANGUAGE FlexibleInstances #-}

module ExprT where

import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Parser (parseExp)
import StackVM (Program, StackExp (..), stackVM)

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

data ExprT
  = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

eval :: ExprT -> Integer
eval (Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit ExprT.Add ExprT.Mul s

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- E5

instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- E6

class HasVars a where
  var :: String -> a

data VarExprT
  = VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add :: VarExprT -> VarExprT -> VarExprT
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance HasVars VarExprT where
  var :: String -> VarExprT
  var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit :: Integer -> M.Map String Integer -> Maybe Integer
  lit x _ = Just x
  mul x y m = (*) <$> x m <*> y m
  add x y m = (+) <$> x m <*> y m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
