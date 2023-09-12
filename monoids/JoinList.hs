{-# LANGUAGE GADTs #-}

module JoinList where

import Scrabble
import Sized
import Text.XHtml (lang)

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
  | otherwise = indexJ (i - subtreeSize x) y

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
    takeRight = takeJ (n - subtreeSize left) right

testTake :: (Eq a, Sized m, Monoid m) => Int -> JoinList m a -> Bool
testTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x