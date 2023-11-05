{-# LANGUAGE OverloadedStrings #-}

module Expr where

import TextShow (TextShow (showb, showbPrec), showbParen)

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

eval :: (Num a) => Expr a -> a
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 + eval e2

instance (TextShow a) => TextShow (Expr a) where
  showbPrec p e =
    case e of
      Lit x -> showb x
      Mult e1 e2 -> showBHelper p 8 "*" e1 e2
      Add e1 e2 -> showBHelper p 4 "+" e1 e2
    where
      showBHelper outerPrec innerPrec op e1 e2 =
        showbParen (outerPrec > innerPrec) $ showbPrec innerPrec e1 <> op <> showbPrec innerPrec e2

expr1, expr2 :: Expr Int
expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)
expr2 =
  Add
    ( Add
        (Lit 1)
        ( Mult
            (Add (Lit 1) (Lit 2))
            ( Add
                (Lit 2)
                (Mult (Lit 2) (Add (Lit 1) (Lit 2)))
            )
        )
    )
    (Add (Lit 1) (Mult (Lit 3) (Lit 2)))