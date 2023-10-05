module Interpreter where

import Syntax
import Text.Parsec (ParseError)

interpret :: Either ParseError ArithE -> Maybe Double
interpret (Left error) = Nothing
interpret (Right a) = Just $ interpret' a

interpret' :: ArithE -> Double
interpret' (LitE (Left i)) = fromIntegral i
interpret' (LitE (Right d)) = d
interpret' (Unary Neg right) = negate $ interpret' right
interpret' (Bin Plus left right) = interpret' left + interpret' right
interpret' (Bin Minus left right) = interpret' left - interpret' right
interpret' (Bin Times left right) = interpret' left * interpret' right
interpret' (Bin Div left right) = interpret' left / interpret' right
interpret' (Bin Exp left right) = interpret' left ** interpret' right
interpret' (Par _ mid _) = interpret' mid