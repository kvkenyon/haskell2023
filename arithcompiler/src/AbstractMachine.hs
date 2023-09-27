module AbstractMachine (Instruction (PUSH, ADD, SUB, MUL), run) where

{--
Our imaginary machine is quite simple. It keeps track of a list of instructions to execute,
and a stack of integers (recall that Haskell lists can also be used as stacks).
There are four instructions it knows how to execute:
PUSH n: given an integer n, push it on top of the stack.
ADD: pop the top two integers off the stack, add them, and push the result back on top of the stack.
     The machine halts with an error if there are fewer than two integers on the stack.
SUB: pop the top two integers, subtract the topmost from the other, and push the result.
MUL: pop the top two integers, multiply them, and push the result.
--}

data Instruction
  = PUSH Integer
  | ADD
  | SUB
  | MUL

type Program = [Instruction]

type Stack = [Integer]

data MachineState = WORKING Program Stack | DONE Stack | ERROR

step :: MachineState -> MachineState
step ERROR = ERROR
step state@(DONE _) = state
step (WORKING [] s) = DONE s
step (WORKING ((PUSH x) : ops) s) = WORKING ops (x : s)
step (WORKING (ADD : _) []) = ERROR
step (WORKING (ADD : _) [_]) = ERROR
step (WORKING (ADD : ops) (s1 : s2 : ss)) = WORKING ops ((s1 + s2) : ss)
step (WORKING (SUB : _) []) = ERROR
step (WORKING (SUB : _) [_]) = ERROR
step (WORKING (SUB : ops) (s1 : s2 : ss)) = WORKING ops ((s2 - s1) : ss)
step (WORKING (MUL : _) []) = ERROR
step (WORKING (MUL : _) [_]) = ERROR
step (WORKING (MUL : ops) (s1 : s2 : ss)) = WORKING ops ((s1 * s2) : ss)

execute :: Program -> MachineState
execute program = state program []
  where
    state p s = case step $ WORKING p s of
      DONE x -> DONE x
      ERROR -> ERROR
      (WORKING p' s') -> state p' s'

run :: Program -> Maybe Integer
run program = case execute program of
  DONE [] -> Nothing
  DONE x -> Just $ head x
  ERROR -> Nothing
  WORKING _ _ -> Nothing