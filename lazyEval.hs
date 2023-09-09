import Data.List (foldl')

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

testFib :: [Bool]
testFib =
  [ fib 0 == 0,
    fib 1 == 1,
    fib 2 == 1,
    fib 3 == 2,
    fib 9 == 34,
    fib 14 == 377
  ]

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: Integer -> [Integer]
fibs2 n = foldl' (\acc@(fn1 : fn2 : _) x -> (fn1 + fn2) : acc) [1, 0] [2 .. n]

fibs3 :: Integer -> [Integer]
fibs3 n = foldl' (\acc x -> acc ++ [last (init acc) + last acc]) [0, 1] [2 .. n]

fastFib :: Integer -> Integer
fastFib n = snd $ foldl' (\acc _ -> (snd acc, uncurry (+) acc)) (0, 1) [2 .. n]

-- Ex. 3
data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x rest) = Cons (f x) (fmap f rest)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamToList :: Stream a -> [a]
streamToList (Cons x rest) = x : streamToList rest

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show stream = show . take 20 $ streamToList stream