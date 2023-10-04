{-# LANGUAGE GADTs #-}

module Calc where

import qualified Data.Map as M
import Interpreter (interpret)
import Parser (arith, pretty)

description :: String
description =
  unlines
    [ "Welcome to my calculator.",
      "This boring message is being shown because",
      "I have not bothered to update it.",
      "Features this calculator supports: none.",
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
