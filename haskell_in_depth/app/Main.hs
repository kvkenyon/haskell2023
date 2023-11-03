module Main where

import System.Environment (getArgs)
import TypeClasses (rotateFromFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    _ ->
      putStrLn $
        "Usage: locator -r filename direction\n"
          ++ "       locator -o filename"
