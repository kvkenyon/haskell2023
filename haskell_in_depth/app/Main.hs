module Main where

import System.Environment (getArgs)
import TypeClasses (orientFromFile, rotateFromFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    ["-o", fname] -> orientFromFile fname
    _ ->
      putStrLn $
        "Usage: locator -r filename direction\n"
          ++ "       locator -o filename"
