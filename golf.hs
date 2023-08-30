module Golf where

-- 0,1 ; 1,3, 2,5, 3,7
-- abcd


every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
        y:ys -> y : every n ys
        [] -> []

skips:: [a] -> [[a]]
skips xs = [every n xs | n <- [1..length xs]]
