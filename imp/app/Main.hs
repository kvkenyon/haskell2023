module Main (main) where

import qualified Data.Map as M
import Interpreter (World (..), initWorld, interpProg)
import Parser (impParser)
import Parsing (parse)
import System.Environment (getArgs)
import Types (checkProg, showTyError)

formatWorld :: World -> String
formatWorld (W m _ o) =
  unlines $
    reverse o
      ++ ["-----"]
      ++ map formatVar (M.assocs m)
formatWorld Error = "Error"

formatVar :: (Show a) => ([Char], a) -> [Char]
formatVar (x, v) = x ++ " -> " ++ show v

run :: String -> IO ()
run fileName = do
  s <- readFile fileName
  case parse impParser s of
    Left err -> print err
    Right p ->
      case checkProg M.empty p of
        Left tyErr -> putStrLn (showTyError tyErr)
        Right _ -> do
          inp <- getContents
          let es = interpProg p (initWorld inp)
          putStr $ formatWorld es

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please provide a file name."
    (fn : _) -> run fn