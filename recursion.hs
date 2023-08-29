
-- Max
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list."
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Replicate
replicate' :: Integer -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- Take
take' :: Integer -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip

zip' :: [a] -> [a] -> [(a,a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = (x == y) || elem' x ys

-- Quick sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger 
    where smaller = [y | y<-xs, y <= x]
          bigger = [y | y<-xs, y > x]