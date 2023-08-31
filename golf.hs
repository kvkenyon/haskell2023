module Golf where

import Data.List (sort,group, find)

-- 0,1 ; 1,3, 2,5, 3,7
-- abcd


every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
        y:ys -> y : every n ys
        [] -> []

skips:: [a] -> [[a]]
skips xs = [every n xs | n <- [1..length xs]]

localMaxima :: (Ord a) => [a] -> [a]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x,y] = []
localMaxima (x:y:z:xs)
        | y > x && y > z = y:localMaxima (y:z:xs)
        | otherwise = localMaxima (y:z:xs)

f :: [[Int]] -> [[Int]]
f [] = []
f xs = foldl (\acc x ->  if null x 
        then acc else head x : acc) [] xs :  
        f (filter (not.null) (map tail (filter (not . null) xs)))

printLine :: [Int] -> String
printLine xs = [c | i <- [0..9], c <- if find (==i) xs == Just i 
    then "*" else " " ] ++ "\n"

histogram :: [Int] -> String
histogram xs = concatMap printLine (reverse . f . group $ sort xs)
                 ++ "==========\n" ++ "0123456789\n"