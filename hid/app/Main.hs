module Main where

import Expr
import TextShow (printT)

main :: IO ()
main = do
  printT expr1
  printT expr2
