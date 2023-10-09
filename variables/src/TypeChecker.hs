{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker where

import qualified Data.Map as M
import Interpreter (Ctx)
import Syntax
import Text.Parsec (ParseError)

bopType :: Op -> (Type, Type, Type)
bopType Plus = (TypeInt, TypeInt, TypeInt)
bopType Minus = (TypeInt, TypeInt, TypeInt)
bopType Times = (TypeInt, TypeInt, TypeInt)
bopType Div = (TypeInt, TypeInt, TypeInt)
bopType Equal = (TypeInt, TypeInt, TypeBool)
bopType Less = (TypeInt, TypeInt, TypeBool)

inferBop :: Ctx -> Arith -> Either TypeError Type
inferBop c (Bin op e1 e2) =
  check c e1 t1
    >> check c e2 t2
    >> Right t3
  where
    (t1, t2, t3) = bopType op

check :: Ctx -> Arith -> Type -> Either TypeError ()
check c expr t =
  infer c expr >>= \t1 ->
    if t1 == t then Right () else Left $ TypeError expr t t1

infer :: Ctx -> Arith -> Either TypeError Type
infer _ (Lit (Number _)) = Right TypeInt
infer _ (Lit (Boolean _)) = Right TypeBool
infer c expr@(Bin {}) = inferBop c expr
infer c (Var name) = case M.lookup name c of
  Just t -> Right t
  Nothing -> Left Undefined
infer c (Let name val expr) = infer c val >>= (\t2 -> infer (M.insert name t2 c) expr)
infer c (If test bT bF) =
  check c test TypeBool >> infer c bT >>= \t1 ->
    infer c bF
      >>= (\t2 -> if t1 == t2 then Right t1 else Left (TypeError bT t1 t2))

inferArith :: Arith -> Either TypeError Type
inferArith = infer M.empty

testInfer :: Either ParseError Arith -> Either TypeError Type
testInfer (Left _) = error "Parse error"
testInfer (Right a) = inferArith a