{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

triangles :: [(Int, Int, Int)]
triangles = [(a, b, c) | a <- [1 .. 10], b <- [1 .. 10], c <- [1 .. 10]]

rightTriangles :: [(Int, Int, Int)]
rightTriangles = [(a, b, c) | (a, b, c) <- triangles, a * a + b * b == c * c, perimeter a b c == 24]

perimeter :: Num a => a -> a -> a -> a
perimeter a b c = a + b + c

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

double :: Num a => a -> a
double n = n * 2

secondToLast :: [a] -> a
secondToLast xs = last (init xs)

dropLastTwo :: [a] -> [a]
dropLastTwo xs = init (init xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | null xs = xs
  | length xs == 1 = xs
  | otherwise = doubleEveryOther (dropLastTwo xs) ++ [double (secondToLast xs), last xs]

sumDigitsOfInteger :: Integer -> Integer
sumDigitsOfInteger x =
  if x == 0
    then 0
    else sumDigitsOfInteger (div x 10) + mod x 10

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sumDigitsOfInteger) 0

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

-- type Peg = String

-- type Move = (Peg, Peg)

-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi 0 _ _ _ = []
-- hanoi 1 a b c = [(a, b)]
-- hanoi 2 a b c = [(a, c), (a, b), (c, b)]
-- hanoi n a b c = hanoi (n - 1) a b c ++ hanoi (n - 1) b c a ++ [(a, b)] ++ hanoi (n - 1) c b a

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src tmp dest = [(src, dest)]
hanoi n src tmp dest =
  hanoi (n - 1) src dest tmp
    ++ [(src, dest)]
    ++ hanoi (n - 1) tmp src dest