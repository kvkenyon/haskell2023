data Maybe a = Just a | Nothing

data Tree a =  Tree {
    val :: a,
    left :: Tree a,
    right :: Tree a
} deriving (Show) 

