-- Reverse
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use product" #-}

freverse :: [a] -> [a]
freverse = foldl (flip (:)) [] 

-- Product
fproduct :: (Num a) => [a] -> a 
fproduct = foldl (*) 1

-- Filter
ffilter :: [a] -> (a -> Bool) -> [a]
ffilter xs p = foldr (\ x acc -> if p x then x:acc else acc) [] xs

-- Last
flast :: [a] -> a
flast = foldl1 (\_ x -> x) 

-- How many elements are needed in a square root sum of natural numbers before it exceeds 1000

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x:takeWhile' p xs else [] 

sqrtSum = length (takeWhile' (< 1000) (scanl (\ acc x -> sqrt x + acc) 0 [1..]))
