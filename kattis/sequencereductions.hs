import Data.List (sort, uncons)
import Data.Maybe (listToMaybe)

readInput :: String -> [Integer]
readInput = tail . map read . words

main :: IO ()
main = interact (writeOutput . solve . sort . readInput)

solve :: [Integer] -> Integer
solve [] = 0
solve [x] = 0
solve (x : y : xs) = reduce + solve (reduce : xs)
  where
    reduce = max x y

writeOutput :: Integer -> String
writeOutput = show
