{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >=>" #-}
module Monads where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)

tick :: State Int Int
tick = do
  x <- get
  put $ x + 1
  return x

tick3 :: State Int Int
tick3 = do
  i <- tick
  tick
  tick
  return i

modify2 :: (s -> s) -> State s ()
modify2 f = do
  x <- get
  put $ f x
  return ()

-- A 'StackProgI a' is a program which returns an 'a' and has access
-- to a mutable stack of Ints.
type StackProgI a = State [Int] a

-- Get the size of the stack.
sizeI :: StackProgI Int
sizeI = do
  length <$> get

-- Push an Int onto the stack.
pushI :: Int -> StackProgI ()
pushI i = do
  xs <- get
  put $ i : xs
  return ()

-- Pop the top Int from the stack and return it. (For now, fail by
-- calling 'error' the stack is empty.)
popI :: StackProgI Int
popI = do
  stack <- get
  case stack of
    [] -> error "Trying to pop from an empty stack"
    x : xs -> do
      put xs
      return x

-- Look at the top Int on the stack without popping it.  (Fail with 'error'
-- if the stack is empty.)
peekI :: StackProgI Int
peekI = do
  stack <- get
  case stack of
    [] -> error "Trying to peek from an empty stack"
    x : _ -> return x

-- Run a 'StackProgI a' starting with the empty stack, returning the
-- produced value of type 'a' along with the final stack state.
runStackProgI :: StackProgI a -> (a, [Int])
runStackProgI s = runState s []

opI :: (Int -> Int -> Int) -> StackProgI ()
opI op = do
  a <- popI
  b <- popI
  pushI (op a b)
  return ()

pushListI :: [Int] -> StackProgI ()
pushListI [] = return ()
pushListI (x : xs) = do
  pushI x
  pushListI xs

crushI :: (Int -> Int -> Int) -> StackProgI ()
crushI f = do
  s <- sizeI
  case s of
    0 -> return ()
    1 -> return ()
    _ -> do
      opI f
      crushI f

r :: StackProgI ()
r = do
  pushListI [1, 2, 3]
  opI (+)

r2 :: StackProgI ()
r2 = do
  pushListI [1, 2, 3, 4, 5, 6, 7, 8]
  crushI (+)

data StackError where
  Underflow :: StackError
  deriving (Show)

type StackProgE el a = ExceptT StackError (State [el]) a

sizeE :: StackProgE el Int
sizeE = length <$> lift get

pushE :: el -> StackProgE el ()
pushE i = do
  xs <- lift get
  lift $ put (i : xs)
  return ()

popE :: StackProgE el el
popE = do
  xs <- lift get
  case xs of
    [] -> throwE Underflow
    (x : rest) -> do
      lift $ put rest
      return x

peekE :: StackProgE el el
peekE = do
  stk <- lift get
  case stk of
    [] -> throwE Underflow
    (x : _) -> return x

runStackProgE :: StackProgE el a -> (Either StackError a, [el])
runStackProgE s = runState (runExceptT s) []

opE :: (el -> el -> el) -> StackProgE el ()
opE op = do
  a <- popE
  b <- popE
  pushE (op a b)
  return ()

pushListE :: [el] -> StackProgE el ()
pushListE [] = return ()
pushListE (x : xs) = do
  pushE x
  pushListE xs

crushE :: (el -> el -> el) -> StackProgE el ()
crushE f = do
  s <- sizeE
  case s of
    0 -> return ()
    1 -> return ()
    _ -> do
      opE f
      crushE f

rE :: StackProgE Int ()
rE = do
  pushListE [1, 2, 3]
  opE (+)

r2E :: StackProgE Int ()
r2E = do
  pushListE [1, 2, 3, 4, 5, 6, 7, 8]
  crushE (+)

data StackProgAST el a where
  -- A simple return value.
  Return :: a -> StackProgAST el a
  -- Push a value on the stack.  This instruction stores the value
  -- to push, and the rest of the program (i.e. it's a node with a
  -- single child node).
  Push :: el -> StackProgAST el a -> StackProgAST el a
  -- Pop a value from the stack.  Stores a function which, when
  -- given the element that is popped, determines the rest of the
  -- program.  Another way to think of it is that a Pop node is like
  -- an infinitely-branching tree node: there is one child AST node
  -- for every possible element that could be popped.
  Pop :: (el -> StackProgAST el a) -> StackProgAST el a
  -- Peek at the value on the top of the stack.
  Peek :: (el -> StackProgAST el a) -> StackProgAST el a
  -- Get the size of the stack.
  Size :: (Int -> StackProgAST el a) -> StackProgAST el a
  deriving (Functor)

-- We get an Applicative instance for free from the Monad instance.
instance Applicative (StackProgAST el) where
  pure :: a -> StackProgAST el a
  pure = Return
  (<*>) :: StackProgAST el (a -> b) -> StackProgAST el a -> StackProgAST el b
  (<*>) = ap

instance Monad (StackProgAST el) where
  return :: a -> StackProgAST el a
  return = pure
  (>>=) :: StackProgAST el a -> (a -> StackProgAST el b) -> StackProgAST el b
  (>>=) (Return x) f = f x
  (>>=) (Push x prog) f = Push x (prog >>= f)
  (>>=) (Pop g) f = Pop (\x -> g x >>= f)
  (>>=) (Peek g) f = Peek (\x -> g x >>= f)
  (>>=) (Size g) f = Size (\x -> g x >>= f)

size :: StackProgAST el Int
size = Size Return

push :: el -> StackProgAST el ()
push x = Push x (return ())

pop :: StackProgAST el el
pop = Pop Return

peek :: StackProgAST el el
peek = Peek Return

op :: (el -> el -> el) -> StackProgAST el ()
op f = do
  a <- pop
  b <- pop
  push (a `f` b)

pushList :: [el] -> StackProgAST el ()
pushList [] = pure ()
pushList (x : xs) = do
  push x
  pushList xs

crush :: (el -> el -> el) -> StackProgAST el ()
crush f = do
  s <- size
  case s of
    0 -> return ()
    1 -> return ()
    _ -> do
      a <- pop
      b <- pop
      push $ f a b
      crush f

interpStackProgE :: StackProgAST el a -> StackProgE el a
interpStackProgE (Return x) = return x
interpStackProgE (Push x prog) = pushE x >> interpStackProgE prog
interpStackProgE (Pop g) = popE >>= \x -> interpStackProgE $ g x
interpStackProgE (Size g) = sizeE >>= \x -> interpStackProgE $ g x
interpStackProgE (Peek g) = peekE >>= \x -> interpStackProgE $ g x

runAsStackProgE :: StackProgAST el a -> (Either StackError a, [el])
runAsStackProgE = runStackProgE . interpStackProgE

rASt :: StackProgAST Int ()
rASt = do
  pushList [1, 2, 3, 4]
  crush (+)

type StackProgW el a = ExceptT StackError (WriterT [String] (State [el])) a

pushW :: (Show el) => el -> StackProgW el ()
pushW x = do
  stack <- lift $ lift get
  (lift . lift . put) (x : stack)
  lift $ tell ["Pushed " ++ show x]

popW :: (Show el) => StackProgW el el
popW = do
  stack <- lift $ lift get
  case stack of
    [] -> throwE Underflow
    (x : xs) -> do
      lift $ lift $ put xs
      lift $ tell ["Popped " ++ show x]
      return x

sizeW :: (Show el) => StackProgW el Int
sizeW = do
  stack <- lift $ lift get
  let sz = length stack
  lift $ tell ["Size is " ++ show sz]
  return sz

peekW :: (Show el) => StackProgW el el
peekW = do
  stack <- lift $ lift get
  case stack of
    [] -> throwE Underflow
    (x : _) -> do
      lift $ tell ["Peeked " ++ show x]
      return x

interpStackProgW :: (Show el) => StackProgAST el a -> StackProgW el a
interpStackProgW (Return x) = return x
interpStackProgW (Push x prog) = pushW x >> interpStackProgW prog
interpStackProgW (Pop g) = popW >>= \x -> interpStackProgW $ g x
interpStackProgW (Size g) = sizeW >>= \x -> interpStackProgW $ g x
interpStackProgW (Peek g) = peekW >>= \x -> interpStackProgW $ g x

runAsStackProgW :: (Show el) => StackProgAST el a -> ((Either StackError a, [String]), [el])
runAsStackProgW s = runState (runWriterT $ runExceptT $ interpStackProgW s) []
