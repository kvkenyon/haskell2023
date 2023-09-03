import Data.List (nub, sort)

fun1 :: [Integer] -> Integer
fun1 = foldr (\x acc -> (x - 2) * acc) 1 . filter even

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n
  | even n = n + fun2' (n `div` 2)
  | otherwise = fun2' (3 * n + 1)

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

fun2 :: Integer -> Integer
fun2 n = sum . filter even $ takeWhile (/= 1) $ iterate collatz n

-- foldTree --> generate a balanced bt from a list of a [a]

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (flip foldingFunction) Leaf
  where
    foldingFunction Leaf val = Node 0 Leaf val Leaf
    foldingFunction (Node 0 l@Leaf val r@Leaf) val' = Node 1 (Node 0 Leaf val' Leaf) val r
    foldingFunction (Node 1 l@(Node {}) val Leaf) val' = Node 1 l val (Node 0 Leaf val' Leaf)
    foldingFunction (Node h l@(Node hl left vall right) val r@(Node hr left' valr right')) val'
      | hl == hr = Node (leftH + 1) insertLeft val r
      | hl > hr = Node (rightH + 1) l val insertRight
      where
        insertLeft@(Node leftH _ _ _) = foldingFunction l val'
        insertRight@(Node rightH _ _ _) = foldingFunction r val'

xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x then acc + 1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Sieve of Sundaram

-- Eliminate all numbers of the form
-- i, j in N where 1 <= i <= j
-- i + j + 2ij <= n

eliminate :: Integer -> [Integer]
eliminate n = nub [i + j + 2 * i * j | i <- [1 .. (div n 2)], j <- [1 .. (div n 2)], (i + j + 2 * i * j) <= n]

sieveOfSundaram :: Integer -> [Integer]
sieveOfSundaram n = [2 * i + 1 | i <- [1 .. (n `div` 2)], i `notElem` ineligible]
  where
    ineligible = eliminate n