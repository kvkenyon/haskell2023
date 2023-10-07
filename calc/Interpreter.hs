module Interpreter where

import Data.Complex (Complex (..), imagPart, realPart)
import Syntax
  ( ArithE (..),
    C (E, Pi),
    F (Cos, Log, Round, Sin, Sqrt, Tan),
    Op (Div, Exp, Minus, Neg, Plus, Times),
  )
import Text.Parsec (ParseError)

type DoubleOrComplex = Either Double (Complex Double)

interpret :: Either ParseError ArithE -> Maybe DoubleOrComplex
interpret (Left error) = Nothing
interpret (Right a) = Just $ interpret' a

interpBin :: (Double -> Double -> Double) -> (Complex Double -> Complex Double -> Complex Double) -> DoubleOrComplex -> DoubleOrComplex -> DoubleOrComplex
interpBin opD opCD (Left d) (Left d') = Left (d `opD` d')
interpBin opD opCD (Left d) (Right cd) = Right ((d :+ 0.0) `opCD` cd)
interpBin opD opCD (Right cd) (Right cd') = Right (cd `opCD` cd')
interpBin opD opCD (Right cd) (Left d) = Right (cd `opCD` (d :+ 0.0))

interpFunc :: (Double -> Double) -> (Complex Double -> Complex Double) -> DoubleOrComplex -> DoubleOrComplex
interpFunc opD opCD (Left d) = Left $ opD d
interpFunc opD opCD (Right cd) = Right $ opCD cd

round' :: DoubleOrComplex -> DoubleOrComplex
round' (Left d) = Left $ (fromIntegral . round) d
round' (Right cd) = Right $ (fromIntegral . round . realPart) cd :+ (fromIntegral . round . imagPart) cd

interpret' :: ArithE -> DoubleOrComplex
interpret' (LitE (Left i)) = Left $ fromIntegral i
interpret' (LitE (Right d)) = Left d
interpret' (Const Pi) = Left pi
interpret' (Const E) = Left $ exp 1
interpret' (Cx d) = Right d
interpret' (Unary Neg right) = (interpFunc negate negate . interpret') right
interpret' (Bin Plus left right) = interpBin (+) (+) (interpret' left) (interpret' right)
interpret' (Bin Minus left right) = interpBin (-) (-) (interpret' left) $ interpret' right
interpret' (Bin Times left right) = interpBin (*) (*) (interpret' left) $ interpret' right
interpret' (Bin Div left right) = interpBin (/) (/) (interpret' left) $ interpret' right
interpret' (Bin Exp left right) = interpBin (**) (**) (interpret' left) $ interpret' right
interpret' (Par _ mid _) = interpret' mid
interpret' (Func Sin _ mid _) = interpFunc sin sin $ interpret' mid
interpret' (Func Cos _ mid _) = interpFunc cos cos $ interpret' mid
interpret' (Func Log _ mid _) = interpFunc log log $ interpret' mid
interpret' (Func Sqrt _ mid _) = interpFunc sqrt sqrt $ interpret' mid
interpret' (Func Round _ mid _) = (round' . interpret') mid
interpret' (Func Tan _ mid _) = (interpFunc tan tan . interpret') mid
