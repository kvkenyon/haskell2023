module Monads where

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
  | abs ((n + l) - r) <= 3 = Just (n + l, r)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
  | abs (l - (n + r)) <= 3 = Just (l, n + r)
  | otherwise = Nothing

(-:) :: a -> (a -> b) -> b
x -: f = f x