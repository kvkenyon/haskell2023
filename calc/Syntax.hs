{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Syntax where

import Data.Complex (Complex)

data Op where
  Plus :: Op
  Minus :: Op
  Times :: Op
  Div :: Op
  Exp :: Op
  Neg :: Op
  As :: Op

instance Show Op where
  show :: Op -> String
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Exp = "^"
  show Neg = "-"
  show As = "as"

data F where
  Sin :: F
  Cos :: F
  Tan :: F
  Log :: F
  Sqrt :: F
  Round :: F

instance Show F where
  show :: F -> String
  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"
  show Log = "log"
  show Sqrt = "sqrt"
  show Round = "round"

data C where
  Pi :: C
  E :: C

instance Show C where
  show :: C -> String
  show Pi = "π"
  show E = "ℇ"

constSymbol :: C -> String
constSymbol Pi = "pi"
constSymbol E = "e"

data Paren = LParen | RParen

instance Show Paren where
  show :: Paren -> String
  show LParen = "("
  show RParen = ")"

data Unit
  = In
  | Ft
  | Mi
  | M
  | Cm
  | Mm

instance Show Unit where
  show :: Unit -> String
  show In = "in"
  show Ft = "ft"
  show Mi = "mi"
  show M = "m"
  show Cm = "cm"
  show Mm = "mm"

data ArithE where
  LitE :: Either Integer Double -> ArithE
  LitUnit :: ArithE -> Unit -> ArithE
  Cx :: Complex Double -> ArithE
  Const :: C -> ArithE
  Func :: F -> Paren -> ArithE -> Paren -> ArithE
  Unary :: Op -> ArithE -> ArithE
  Bin :: Op -> ArithE -> ArithE -> ArithE
  Par :: Paren -> ArithE -> Paren -> ArithE
  AsE :: Unit -> ArithE -> ArithE -> ArithE
  deriving (Show)