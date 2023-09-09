{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Lazy where

import Data.List (foldl')

-- Ex. 1

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

-- Ex. 2

fibs2 :: Integer -> [Integer]
fibs2 n = foldl' (\acc@(fn1 : fn2 : _) x -> (fn1 + fn2) : acc) [1, 0] [2 .. n]

fibs3 :: Integer -> [Integer]
fibs3 n = foldl' (\acc x -> acc ++ [last (init acc) + last acc]) [0, 1] [2 .. n]

fastFib :: Integer -> Integer
fastFib n = snd $ foldl' (\acc _ -> (snd acc, uncurry (+) acc)) (0, 1) [2 .. n]

-- Ex. 3/4

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x rest) = Cons (f x) (fmap f rest)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamToList :: Stream a -> [a]
streamToList (Cons x rest) = x : streamToList rest

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show stream = show . take 32 $ streamToList stream

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed uf seed = Cons seed $ streamFromSeed uf (uf seed)

-- Ex. 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

positive :: Stream Integer
positive = streamFromSeed (+ 1) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

-- 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
-- 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64
-- 1 2 1 3 1  2  1  4  1  2  1  3  1  2  1  5  1  2  1  3  1  2  1  4  1  2  1  3  1  2  1  6
--   2 . 3 .  2 .   4 .   2 .   3 .   2 .   5 .   2     3 .   2     4     2     3     2     6
-- 0 1   3 .        7                      15                                               31

-- 0 0 0 0 0 0 0
-- 1 1 1 1 1 1 1
-- 2 2 2 2 2 2 2  interleave = 2 3 2 4 2 3 2 5 2 3 2 4 2 3 2 6
-- 3 3 3 3 3 3 3  interleave = 3 4 3 5 3 4 3 6 3 4 3 7 3 4 3 8 3 4 3 9 ...
-- 4 4 4 4 4 4 4  interleave = 4 5 4 6 4 7 4 8 4 9 ....
-- 5 5 5 5 5 5 5

-- evens = 2k, 0<=k
ruler :: Stream Integer
ruler = interLeaveWith 0

interLeaveWith :: Integer -> Stream Integer
interLeaveWith 10 = streamRepeat 10
interLeaveWith n = interleaveStreams (streamRepeat n) (interLeaveWith $ n + 1)

testRuler' :: Int -> [Bool]
testRuler' n = zipWith (\exp k -> k `mod` (2 ^ exp) == 0) powersOf2 positives
  where
    powersOf2 = take n $ streamToList ruler
    positives = take n $ streamToList positive

testRuler :: [Bool]
testRuler =
  [ and $ testRuler' 20,
    and $ testRuler' 30,
    and $ testRuler' 40,
    and $ testRuler' 50,
    and $ testRuler' 60,
    and $ testRuler' 70,
    and $ testRuler' 100,
    and $ testRuler' 500,
    and $ testRuler' 1000,
    and $ testRuler' 2000,
    and $ testRuler' 10000,
    and $ testRuler' 100000,
    and $ testRuler' 1000000
  ]

-- Ex. 6

x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

mul :: Integer -> Stream Integer -> Stream Integer
mul a (Cons b b') = Cons (a * b) (mul a b')

instance Num (Stream Integer) where
  negate :: Stream Integer -> Stream Integer
  negate (Cons x xs) = Cons (-1 * x) (negate xs)

  (+) :: Stream Integer -> Stream Integer -> Stream Integer
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) $ xs + ys

  (*) :: Stream Integer -> Stream Integer -> Stream Integer
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (mul a0 b' + a' * b)

  fromInteger :: Integer -> Stream Integer
  fromInteger n = Cons n $ streamRepeat 0

-- Q = (a0/b0) + x((1/b0)(A' − QB'))
q :: Stream Integer -> Stream Integer -> Stream Integer
q a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0) (mul (1 `div` b0) (a' + (negate (q a b) * b')))

instance Fractional (Stream Integer) where
  (/) :: Stream Integer -> Stream Integer -> Stream Integer
  (/) = q

fibs4 :: Stream Integer
fibs4 = x / (1 - x - x ^ 2)