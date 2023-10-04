-- The main calculator REPL.  You do not need to edit this file!

import           Calc

import           Data.List                (isPrefixOf)
import           System.Console.Haskeline

main :: IO ()
main = putStrLn description >> runInputT settings loop
  where
    settings = defaultSettings { historyFile = Just ".calc-hist" }

    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing      -> return ()
        Just s | s `isPrefixOf` ":quit" -> return ()
               | s `isPrefixOf` ":help" -> (outputStrLn $ helpMsg) >> loop
        Just input   -> do
          outputStrLn $ calc input
          loop
