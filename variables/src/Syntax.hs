{-# LANGUAGE GADTs #-}

module Syntax where

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
