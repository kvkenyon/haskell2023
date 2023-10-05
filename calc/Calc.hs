{-# LANGUAGE GADTs #-}

module Calc where

import qualified Data.Map as M
import Interpreter (interpret)
import Parser (arith, pretty)

description :: String
description =
  unlines
    [ "KWK Limited Company",
      "The Rand Calculator",
      "Ultimate computational computing machine",
      "Features this calculator supports: Add, Sub, Exp, Times, Neg and more.",
      "Type an expression, :help, or :quit."
    ]

helpMsg :: String
helpMsg =
  unlines
    [ "You can use integers or floating point values,",
      "negation, or standard arithmetic operators + - * / ^ ."
    ]

calc :: String -> String
calc input =
  let result = (interpret . arith) input; s = prettyPrint input
   in s ++ "\n" ++ "  =" ++ show result

prettyPrint :: String -> String
prettyPrint input = out
  where
    result = pretty input
    out
      | (Left err) <- result = show err
      | (Right d) <- result = d
