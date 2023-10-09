{-# LANGUAGE PartialTypeSignatures #-}

import Data.List (isPrefixOf)
import qualified Data.Map as M
import Interpreter (interpArith, showInterpError)
import Parser (arith)
import Parsing (parse)
import System.Console.Haskeline

description :: String
description =
  unlines
    [ "VarX Lang",
      "Type an expression, :help, or :quit."
    ]

helpMsg :: String
helpMsg =
  unlines
    [ "You can use integers or boolean values,",
      ",standard arithmetic operators + - * / ^, let bindings, and if then else . VarX Lang is goated."
    ]

eval :: String -> String
eval s = do
  case parse arith s of
    Left parseError -> show parseError
    Right ast -> case interpArith M.empty ast of
      Left interpreterError -> showInterpError interpreterError
      Right val -> show val

main :: IO ()
main = putStrLn description >> runInputT settings loop
  where
    settings = defaultSettings {historyFile = Just ".calc-hist"}

    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just s
          | s `isPrefixOf` ":quit" -> return ()
          | s `isPrefixOf` ":help" -> outputStrLn helpMsg >> loop
        Just input -> do
          outputStrLn $ eval input
          loop
