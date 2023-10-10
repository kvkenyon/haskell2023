{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Interpreter where

import qualified Data.Map as M
import Syntax
import Prelude hiding (pred)

type Env = M.Map String Value

type Ctx = M.Map String Type

data InterpError = DivByZero

interpArith :: Env -> Arith -> Either InterpError Integer
interpArith _ (Lit (Number x)) = Right x
interpArith _ (Lit (Boolean x)) = Right (if x then 1 else 0)
interpArith e (Var x) = case M.lookup x e of
  Just (Number i) -> Right i
  Just (Boolean b) -> Right (if b then 1 else 0)
  Nothing -> error "This is not possible due to type checking"
interpArith e (Let v a1 a2) = interpArith e a1 >>= (\x -> interpArith (M.insert v (Number x) e) a2)
interpArith e (If test a b) = interpArith e test >>= (\x -> if x == 1 then interpArith e a else interpArith e b)
interpArith e (Bin Plus e1 e2) = (+) <$> interpArith e e1 <*> interpArith e e2
interpArith e (Bin Minus e1 e2) = (-) <$> interpArith e e1 <*> interpArith e e2
interpArith e (Bin Times e1 e2) = (*) <$> interpArith e e1 <*> interpArith e e2
interpArith e (Bin Div e1 e2) = do
  numerator <- interpArith e e1
  denom <- interpArith e e2
  case denom of
    0 -> Left DivByZero
    _ -> Right $ div numerator denom
interpArith e (Bin Equal e1 e2) = interpArith e e1 >>= (\r1 -> interpArith e e2 >>= \r2 -> if r1 == r2 then Right 1 else Right 0)
interpArith e (Bin Less e1 e2) = interpArith e e1 >>= (\r1 -> interpArith e e2 >>= \r2 -> if r1 < r2 then Right 1 else Right 0)

showInterpError :: InterpError -> String
showInterpError DivByZero = "Runtime Error: Division by zero"
