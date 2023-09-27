
zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = g
    where g = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a-> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

collatz :: Int-> [Int]
collatz 1 = [1]
collatz n
    | odd n = threeNPlus1 : collatz threeNPlus1
    | otherwise = divBy2 : collatz divBy2
        where threeNPlus1 = 3*n+1
              divBy2 = n `div` 2

collatzLengthLessThan :: Int -> Int
collatzLengthLessThan n = length (filter (<= n) (map (length . collatz) [1..100]))

collatzLengthGreaterThan :: Int -> Int
collatzLengthGreaterThan n = length (filter isLengthGreater (map collatz [1..100]))
    where isLengthGreater xs = length xs > n


