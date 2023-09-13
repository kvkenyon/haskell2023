module Party where

import Data.Tree (Tree (Node))
import Employee

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL gl f) b@(GL gl' f')
  | f < f' = b
  | otherwise = a

instance Semigroup GuestList where
  (<>) :: GuestList -> GuestList -> GuestList
  (<>) = moreFun

instance Monoid GuestList where
  mempty :: GuestList
  mempty = GL [] 0
  mappend :: GuestList -> GuestList -> GuestList
  mappend = (<>)
  mconcat :: [GuestList] -> GuestList
  mconcat = foldl (<>) mempty

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es funScore) = GL (e : es) (funScore + empFun e)

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f acc (Node root []) = f acc root
treeFold f acc (Node root forest) = foldl (treeFold f) acc' forest
  where
    acc' = f acc root

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss ((withBoss, withoutBoss) : rest) = ()