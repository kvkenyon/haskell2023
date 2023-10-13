{-# LANGUAGE GADTs #-}

module Interpreter where

import qualified Data.Map as M
import Syntax
import Text.Read (readMaybe)

type Value = Integer

type Mem = M.Map Var Value

interpExpr :: Mem -> Expr -> Value
interpExpr _ (EInt i) = i
interpExpr _ (EBool b) = fromBool b
interpExpr m (EVar x) =
  case M.lookup x m of
    Just v -> v
    Nothing -> error $ "Impossible! Uninitialized variable " ++ x
interpExpr m (EBin b e1 e2) = interpBOp b (interpExpr m e1) (interpExpr m e2)
interpExpr m (EUn u e) = interpUOp u (interpExpr m e)

interpUOp :: UOp -> Value -> Value
interpUOp Neg v = -v
interpUOp Not v = 1 - v

interpBOp :: BOp -> Value -> Value -> Value
interpBOp Add = (+)
interpBOp Sub = (-)
interpBOp Mul = (*)
interpBOp Div = div
interpBOp And = (*)
interpBOp Or = \v1 v2 -> min 1 (v1 + v2)
interpBOp Equals = \v1 v2 -> fromBool (v1 == v2)
interpBOp Less = \v1 v2 -> fromBool (v1 < v2)

fromBool :: Bool -> Value
fromBool False = 0
fromBool True = 1

data World where
  W ::
    Mem -> -- Current state of memory
    [String] -> -- Strings typed by the user, waiting to be read by 'input'
    [String] -> -- Strings produced by 'output' (newest first)
    World
  Error :: World -- Something went wrong
  deriving (Show)

-- An initial world state, given user input
initWorld :: String -> World
initWorld inp = W M.empty (words inp) []

interpStmt :: Stmt -> World -> World
interpStmt _ Error = Error
interpStmt (Decl _ _) w = w
interpStmt (Assign var expr) (W mem input output) = w'
  where
    result = interpExpr mem expr
    mem' = M.insert var result mem
    w' = W mem' input output
interpStmt (Block stmts) w = interpProg stmts w
interpStmt (If expr stmt1 stmt2) w@(W mem _ _) =
  if result == 1
    then interpStmt stmt1 w
    else interpStmt stmt2 w
  where
    result = interpExpr mem expr
interpStmt (Repeat expr stmt) w@(W mem _ _) =
  interpRepeat (interpExpr mem expr) stmt w
interpStmt while@(While expr stmt) w@(W mem _ _) =
  if result mem == 1 then interpStmt while (interpStmt stmt w) else w
  where
    result m = interpExpr m expr
interpStmt (Input _) (W _ [] _) = Error
interpStmt (Input var) (W mem (i : ii) o) = interp
  where
    interp = case readMaybe i of
      Just n -> W (mem' n) ii o
      Nothing -> Error
    mem' input = M.insert var input mem
interpStmt (Output expr) (W mem i o) = W mem i (show result : o)
  where
    result = interpExpr mem expr

interpRepeat :: Integer -> Stmt -> World -> World
interpRepeat 0 _ w = w
interpRepeat n s w = interpRepeat (n - 1) s (interpStmt s w)

interpProg :: Prog -> World -> World
interpProg [] w = w
interpProg stmts w = foldl (flip interpStmt) w stmts