{-# LANGUAGE GADTs #-}

module Calc where

import ArithParser (arith)
import Data.Complex (imagPart, realPart)
import qualified Data.Map as M
import Interpreter (DoubleOrComplex, interpret)
import PrettyParser (pretty)

description :: String
description =
  unlines
    [ "KWK Limited Company",
      "The Calculator",
      "Features this calculator supports: Add, Sub, Exp, Times, Neg and more.",
      "Type an expression, :help, or :quit."
    ]

helpMsg :: String
helpMsg =
  unlines
    [ "You can use integers or floating point values,",
      "negation, or standard arithmetic operators + - * / ^ . Calc",
      "also supports functions: sin, cos, tan, sqrt, log, and round. Constants e and pi."
    ]

calc :: String -> String
calc input =
  let result = (interpret . arith) input; s = prettyPrint input
   in s ++ "\n" ++ "  =" ++ prettyPrintResult result

prettyPrintResult :: Maybe DoubleOrComplex -> String
prettyPrintResult Nothing = "Error"
prettyPrintResult (Just (Left d)) = show d
prettyPrintResult (Just (Right cd)) = (show . realPart) cd ++ " + " ++ (show . imagPart) cd ++ "i"

prettyPrint :: String -> String
prettyPrint input = out
  where
    result = pretty input
    out
      | (Left err) <- result = show err
      | (Right d) <- result = d
