module Interpreter where

import Syntax
import Text.Parsec (ParseError)

interpret :: Either ParseError ArithE -> Maybe Double
interpret (Left error) = Nothing
interpret (Right a) = Just $ interpret' a

interpret' :: ArithE -> Double
interpret' (LitE (Left i)) = fromIntegral i
interpret' (LitE (Right d)) = d
interpret' (Const Pi) = pi
interpret' (Const E) = exp 1
interpret' (Unary Neg right) = negate $ interpret' right
interpret' (Bin Plus left right) = interpret' left + interpret' right
interpret' (Bin Minus left right) = interpret' left - interpret' right
interpret' (Bin Times left right) = interpret' left * interpret' right
interpret' (Bin Div left right) = interpret' left / interpret' right
interpret' (Bin Exp left right) = interpret' left ** interpret' right
interpret' (Par _ mid _) = interpret' mid
interpret' (Func Sin _ mid _) = sin $ interpret' mid
interpret' (Func Cos _ mid _) = cos $ interpret' mid
interpret' (Func Log _ mid _) = log $ interpret' mid
interpret' (Func Sqrt _ mid _) = sqrt $ interpret' mid
interpret' (Func Round _ mid _) = (fromIntegral . round . interpret') mid
interpret' (Func Tan _ mid _) = (tan . interpret') mid
