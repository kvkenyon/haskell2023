module ConvexHull where
import Data.List (sortBy)

length' :: [a] -> Integer
length' = foldr (\_ acc -> acc + 1) 0

mean :: (Real a) => [a] -> Double
mean xs =  realToFrac (sum xs) / fromIntegral (length' xs)

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []     = True
isPalindrome [x]    = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\x y -> compare (length' x) (length' y))