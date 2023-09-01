-- product (x-2) x is even for all x in xs

fun1 :: [Integer] -> Integer
fun1 = foldr (\x acc -> (x-2) * acc) 1 . filter even

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

collatz:: Integer -> Integer
collatz n | even n    = n `div` 2
          | otherwise = 3*n+1

fun2 :: Integer -> Integer
fun2 n = sum . filter even $ takeWhile (/=1) $ iterate collatz n
