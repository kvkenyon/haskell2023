module Party where

import Data.List (sort)
import Data.Tree (Tree (Node))
import Employee (Employee (empFun, empName), GuestList (..))

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b
  | a < b = b
  | otherwise = a

combine :: GuestList -> GuestList -> GuestList
combine (GL emps fun) (GL emps' fun') =
  GL (emps ++ emps') (fun + fun')

instance Semigroup GuestList where
  (<>) :: GuestList -> GuestList -> GuestList
  (<>) = combine

instance Monoid GuestList where
  mempty :: GuestList
  mempty = GL [] 0
  mappend :: GuestList -> GuestList -> GuestList
  mappend = (<>)
  mconcat :: [GuestList] -> GuestList
  mconcat = foldl (<>) mempty

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es funScore) = GL (e : es) (funScore + empFun e)

treeFold :: b -> (b -> a -> b) -> ([b] -> b) -> Tree a -> b
treeFold e f g (Node root []) = f e root
treeFold e f g (Node root forest) =
  f
    (g (map (treeFold e f g) forest))
    root

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = (GL [emp] (empFun emp), GL [] 0)
nextLevel boss subtreeGuestLists = (withBoss, withoutBoss)
  where
    withBoss = glCons boss $ mconcat $ map snd subtreeGuestLists
    withoutBoss = mconcat $ map fst subtreeGuestLists

nextLevel' :: [(GuestList, GuestList)] -> Employee -> [(GuestList, GuestList)]
nextLevel' x y = [nextLevel y x]

maxFun :: Tree Employee -> GuestList
maxFun company =
  uncurry
    moreFun
    (head $ treeFold [] nextLevel' mconcat company)

-- Create output for main

formattedOutput :: String -> String
formattedOutput s = funOutput ++ empOutput
  where
    company = read s :: Tree Employee
    (GL emps fun) = maxFun company
    funOutput = "Total fun: " ++ show fun ++ "\n"
    empOutput = unlines (sort $ map empName emps)

main :: IO ()
main = do
  companyStr <- readFile "company.txt"
  putStrLn $ formattedOutput companyStr