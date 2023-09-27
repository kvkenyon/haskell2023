{-# LANGUAGE GADTs #-}

module Syntax
  ( Op
      ( Plus,
        Minus,
        Times
      ),
    Arith (Lit, Bin),
  )
where

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  deriving (Show)
