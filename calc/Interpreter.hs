module Interpreter where

import Syntax (Arith (..))
import Text.Parsec (ParseError)

interpret :: Either ParseError Arith -> Maybe Double
interpret (Left error) = Nothing
interpret (Right a) = Just $ interpret' a

interpret' :: Arith -> Double
interpret' (Lit (Left i)) = fromIntegral i
interpret' (Lit (Right d)) = d
interpret' (Neg right) = negate $ interpret' right
interpret' (Add left right) = interpret' left + interpret' right
interpret' (Subt left right) = interpret' left - interpret' right
interpret' (Mult left right) = interpret' left * interpret' right
interpret' (Div left right) = interpret' left / interpret' right
interpret' (Exp left right) = interpret' left ** interpret' right
