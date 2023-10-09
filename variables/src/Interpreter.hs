{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Interpreter where

import qualified Data.Map as M
import Syntax
import Prelude hiding (pred)

type Env = M.Map String Value

type Ctx = M.Map String Type

data InterpError = UndefinedVar String | DivByZero | ITypeError

intOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Either InterpError Integer
intOp op (Number a) (Number b) = Right $ op a b
intOp _ _ _ = Left ITypeError

boolOp :: (Integer -> Integer -> Bool) -> Value -> Value -> Either InterpError Bool
boolOp op (Number a) (Number b) = Right $ op a b
boolOp _ _ _ = Left ITypeError

interpBool :: Env -> Arith -> Either InterpError Bool
interpBool e (Bin Equal e1 e2) = interpArith e e1 >>= (\a -> interpArith e e2 >>= \b -> boolOp (==) a b)
interpBool e (Bin Less e1 e2) = interpArith e e1 >>= (\a -> interpArith e e2 >>= \b -> boolOp (<) a b)

interpInteger :: Env -> Arith -> Either InterpError Integer
interpInteger e (Bin Plus e1 e2) = interpArith e e1 >>= (\a -> interpArith e e2 >>= \b -> intOp (+) a b)
interpInteger e (Bin Minus e1 e2) = interpArith e e1 >>= (\a -> interpArith e e2 >>= \b -> intOp (-) a b)
interpInteger e (Bin Times e1 e2) = interpArith e e1 >>= (\a -> interpArith e e2 >>= \b -> intOp (*) a b)
interpInteger e (Bin Div e1 e2) = do
  numerator <- interpArith e e1
  denom <- interpArith e e2
  case denom of
    (Number 0) -> Left DivByZero
    v -> intOp div numerator v

interpArith :: Env -> Arith -> Either InterpError Value
interpArith _ (Lit i) = Right i
interpArith e (Var v) = case M.lookup v e of
  Just i -> Right i
  Nothing -> Left $ UndefinedVar v
interpArith e (Let v a1 a2) = interpArith e a1 >>= (\x -> interpArith (M.insert v x e) a2)
interpArith e (If cond bTrue bFalse) = do
  pred <- interpBool e cond
  (if pred then interpArith e bTrue else interpArith e bFalse) >>= \x -> Right x
interpArith e a@(Bin Plus _ _) = interpInteger e a >>= (return . Number)
interpArith e a@(Bin Times _ _) = interpInteger e a >>= (return . Number)
interpArith e a@(Bin Minus _ _) = interpInteger e a >>= (return . Number)
interpArith e a@(Bin Div _ _) = interpInteger e a >>= (return . Number)
interpArith e b@(Bin Equal _ _) = interpBool e b >>= (return . Boolean)
interpArith e b@(Bin Less _ _) = interpBool e b >>= (return . Boolean)

showInterpError :: InterpError -> String
showInterpError (UndefinedVar x) = "Runtime Error: Undefined variable " ++ x
showInterpError DivByZero = "Runtime Error: Division by zero"
showInterpError ITypeError = "Type Error"
