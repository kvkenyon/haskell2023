module Main where

import Editor
import JoinListBuffer

main :: IO ()
main =
  runEditor editor initBuff