{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Syntax where

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  Div :: Op
  Exp :: Op
  Neg :: Op

instance Show Op where
  show :: Op -> String
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Exp = "^"
  show Neg = "-"

data Paren = LParen | RParen

instance Show Paren where
  show :: Paren -> String
  show LParen = "("
  show RParen = ")"

data ArithE where
  LitE :: Either Integer Double -> ArithE
  Unary :: Op -> ArithE -> ArithE
  Bin :: Op -> ArithE -> ArithE -> ArithE
  Par :: Paren -> ArithE -> Paren -> ArithE
  deriving (Show)