
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Ord, Show, Read)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node x left right) y
    | x == y = Node y left right
    | x < y  = Node x left (insert right y) 
    | otherwise = Node x (insert left y) right

search :: (Eq b) => Tree b -> b -> Either String b
search Leaf _ = Left "Value not found." 
search (Node x left right) y
    | x == y = Right x
    | otherwise = if searchLeft == notFound then searchRight else searchLeft 
        where searchLeft = search left y
              searchRight = search right y
              notFound = Left "Value not found." 

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf 
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right) 