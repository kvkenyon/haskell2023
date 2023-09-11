{-# LANGUAGE GADTs #-}

module JoinList where

import Sized
import Text.XHtml (lang)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score x) = x

instance Semigroup Score where
  (<>) :: Score -> Score -> Score
  (<>) = (+)

instance Monoid Score where
  mempty :: Score
  mempty = Score 0
  mappend :: Score -> Score -> Score
  mappend = (<>)

onePoint = ['A', 'a', 'E', 'e', 'I', 'i', 'L', 'l', 'N', 'n', 'O', 'o', 'R', 'r', 'S', 's', 'T', 't', 'U', 'u']

twoPoint = ['D', 'd', 'G', 'g']

threePoint = ['C', 'c', 'M', 'm', 'P', 'p']

fourPoint = ['F', 'f', 'v', 'V', 'y', 'Y']

fivePoint = ['K', 'k']

eightPoint = ['X', 'x', 'J', 'j']

tenPoint = ['Z', 'z', 'Q', 'q']

score :: Char -> Score
score 'A' = 1
score 'a' = 1

data JoinList m a where
  Empty :: JoinList m a
  Single :: m -> a -> JoinList m a
  Append :: m -> JoinList m a -> JoinList m a -> JoinList m a
  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty j = j
(+++) j Empty = j
(+++) left right = Append (tag left <> tag right) left right

single :: a -> JoinList Size a
single = Single (Size 1)

append :: JoinList Size a -> JoinList Size a -> JoinList Size a
append x y = Append (tag x <> tag y) x y

subtreeSize :: (Sized b, Monoid b) => JoinList b a -> Int
subtreeSize x = getSize (size $ tag x)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Single _ _) = Nothing
indexJ i (Append _ x y)
  | i < subtreeSize x = indexJ i x
  | otherwise = indexJ (i - subtreeSize y) y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ n (Single _ _) = Empty
dropJ n (Append v left right)
  | subtreeSize left <= n = dropJ (n - subtreeSize left) right
  | otherwise = Append (tag dropLeft <> tag right) dropLeft right
  where
    dropLeft = dropJ n left

testDrop :: (Eq a, Sized m, Monoid m) => Int -> JoinList m a -> Bool
testDrop n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ n s@(Single _ _) = if n < 0 then Empty else s
takeJ n (Append v left right)
  | n < 0 = Empty
  | subtreeSize left <= n = Append (tag left <> tag takeRight) left takeRight
  | otherwise = takeJ n left
  where
    takeRight = takeJ (n - subtreeSize left) left

testTake :: (Eq a, Sized m, Monoid m) => Int -> JoinList m a -> Bool
testTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)