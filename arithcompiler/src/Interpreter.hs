module Interpreter (interp) where

import Syntax (Arith (..), Op (..))

interp :: Arith -> Integer
interp (Lit n) = n
interp (Bin op a1 a2) = interpOp op (interp a1) (interp a2)

interpOp :: Op -> Integer -> Integer -> Integer
interpOp Plus = (+)
interpOp Minus = (-)
interpOp Times = (*)