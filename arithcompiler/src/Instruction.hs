module Instruction (Instruction (PUSH, ADD, SUB, MUL)) where

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