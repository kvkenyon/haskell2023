{-# LANGUAGE GADTs #-}

module Syntax where

type Var = String

type Prog = [Stmt]

data Type where
  TyInt :: Type
  TyBool :: Type
  deriving (Show, Eq)

data Stmt where
  Decl :: Type -> Var -> Stmt -- <type> <var>
  Assign :: Var -> Expr -> Stmt -- <var> ':=' <expr>
  Block :: Prog -> Stmt -- '{' <prog> '}'
  If :: Expr -> Stmt -> Stmt -> Stmt -- 'if' <expr> 'then' <stmt> 'else' <stmt>
  Repeat :: Expr -> Stmt -> Stmt -- 'repeat' <expr> <stmt>
  While :: Expr -> Stmt -> Stmt -- 'while' <expr> <stmt>
  Input :: Var -> Stmt -- 'input' <var>
  Output :: Expr -> Stmt -- 'output' <expr>
  deriving (Show)

data Expr where
  EInt :: Integer -> Expr -- <int>
  EBool :: Bool -> Expr -- 'False' | 'True'
  EVar :: Var -> Expr -- <var>
  EUn :: UOp -> Expr -> Expr -- <uop> <expr>
  EBin :: BOp -> Expr -> Expr -> Expr -- <expr> <bop> <expr>
  deriving (Show)

data UOp = Neg | Not
  deriving (Show, Eq)

data BOp = Add | Sub | Mul | Div | And | Or | Equals | Less
  deriving (Show, Eq)
