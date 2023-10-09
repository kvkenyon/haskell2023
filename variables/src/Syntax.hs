{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Syntax where

data Type = TypeInt | TypeBool
  deriving (Show, Eq)

data TypeError where
  Undefined :: TypeError
  TypeError :: Arith -> Type -> Type -> TypeError

instance Show TypeError where
  show :: TypeError -> String
  show Undefined = "Undefined"
  show (TypeError _ e a) = "Type Error: in <expr> expected type " ++ show e ++ " got type " ++ show a

data Value where
  Number :: Integer -> Value
  Boolean :: Bool -> Value
  deriving (Show, Eq, Ord)

data Arith where
  Lit :: Value -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  Var :: String -> Arith
  Let :: String -> Arith -> Arith -> Arith
  If :: Arith -> Arith -> Arith -> Arith
  deriving (Show)

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  Div :: Op
  Less :: Op
  Equal :: Op
  deriving (Show, Eq)
