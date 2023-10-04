{-# LANGUAGE GADTs #-}

module Syntax where

data Arith where
  Lit :: Either Integer Double -> Arith
  Add :: Arith -> Arith -> Arith
  Mult :: Arith -> Arith -> Arith
  Subt :: Arith -> Arith -> Arith
  Div :: Arith -> Arith -> Arith
  Exp :: Arith -> Arith -> Arith
  Neg :: Arith -> Arith
  deriving (Show)
