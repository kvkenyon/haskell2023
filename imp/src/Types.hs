{-# LANGUAGE GADTs #-}

module Types where

import qualified Data.Map as M
import Syntax

data TypeError where
  DuplicateVar :: Var -> TypeError
  UndefinedVar :: Var -> TypeError
  Mismatch :: Expr -> Type -> Type -> TypeError
  InputBool :: Var -> TypeError
  deriving (Show)

showTyError :: TypeError -> String
showTyError (DuplicateVar x) = "Duplicate variable declaration: " ++ x
showTyError (UndefinedVar x) = "Variable used before declaration: " ++ x
showTyError (Mismatch e ty1 ty2) =
  unlines
    [ "Type mismatch in expression " ++ show e,
      "  expected " ++ show ty1,
      "  but got " ++ show ty2 ++ " instead."
    ]
showTyError (InputBool _) = "Cannot 'input' a boolean variable."

type Ctx = M.Map Var Type

infer :: Ctx -> Expr -> Either TypeError Type
infer _ (EInt _) = Right TyInt -- Integers have type int
infer _ (EBool _) = Right TyBool -- Booleans have type bool
infer ctx (EVar x) =
  -- Look up the type of variables
  case M.lookup x ctx of --   in the context
    Nothing -> Left $ UndefinedVar x
    Just ty -> Right ty
infer ctx (EBin op e1 e2) = inferBin ctx op e1 e2 -- Call helper functions for
infer ctx (EUn op e) = inferUn ctx op e -- binary & unary operators

binTy :: BOp -> (Type, Type, Type) -- (input1, input2, output)
binTy op
  | op `elem` [Add, Sub, Mul, Div] = (TyInt, TyInt, TyInt)
  | op `elem` [And, Or] = (TyBool, TyBool, TyBool)
  | op `elem` [Equals, Less] = (TyInt, TyInt, TyBool)
  | otherwise = error "Unhandled operator in binTy"

inferBin :: Ctx -> BOp -> Expr -> Expr -> Either TypeError Type
inferBin ctx op e1 e2 =
  case binTy op of
    (ty1, ty2, tyOut) ->
      check ctx e1 ty1
        *> check ctx e2 ty2
        *> Right tyOut

unTy :: UOp -> (Type, Type)
unTy Neg = (TyInt, TyInt)
unTy Not = (TyBool, TyBool)

inferUn :: Ctx -> UOp -> Expr -> Either TypeError Type
inferUn ctx op e =
  case unTy op of
    (tyIn, tyOut) ->
      check ctx e tyIn
        *> Right tyOut

check :: Ctx -> Expr -> Type -> Either TypeError ()
check ctx e ty =
  infer ctx e >>= \ty' ->
    (if ty == ty' then Right () else Left $ Mismatch e ty ty')

-- My code

checkProg :: Ctx -> Prog -> Either TypeError Ctx
checkProg ctx [] = Right ctx
checkProg ctx (s : ss) = checkStmt ctx s >>= (`checkProg` ss)

checkStmt :: Ctx -> Stmt -> Either TypeError Ctx
checkStmt ctx (Decl ty x) = case M.lookup x ctx of
  Just _ -> Left $ DuplicateVar x
  Nothing -> return $ M.insert x ty ctx
checkStmt ctx (Assign x e) = case M.lookup x ctx of
  Just ty -> check ctx e ty >> Right ctx
  Nothing -> Left $ UndefinedVar x
checkStmt ctx (Block ss) = checkProg ctx ss *> Right ctx
checkStmt ctx (If e s1 s2) =
  check ctx e TyBool
    *> checkStmt ctx s1
    *> checkStmt ctx s2
    *> Right ctx
checkStmt ctx (Repeat e body) =
  check ctx e TyInt
    *> checkStmt ctx body
    *> Right ctx
checkStmt ctx (While e body) =
  check ctx e TyBool
    *> checkStmt ctx body
    *> Right ctx
checkStmt ctx (Input v) =
  case M.lookup v ctx of
    Nothing -> Left $ UndefinedVar v
    Just TyInt -> Right ctx
    Just _ -> Left $ InputBool v
checkStmt ctx (Output e) =
  check ctx e TyInt *> Right ctx