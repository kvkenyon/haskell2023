import System.Directory.Internal.Prelude (getArgs)

solveRPN :: String -> Float
solveRPN xs = head . foldl foldingFunction [] $ words xs
  where
    foldingFunction (x : y : ys) "+" = x + y : ys
    foldingFunction (x : y : ys) "-" = y - x : ys
    foldingFunction (x : y : ys) "*" = x * y : ys
    foldingFunction (x : y : ys) "/" = y / x : ys
    foldingFunction (x : y : ys) "^" = (y ** x) : ys
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs x = read x : xs

main :: IO () =
  do
    expr <- getArgs
    print $ solveRPN $ unwords expr