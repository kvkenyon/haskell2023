data Maybe a = Just a | Nothing

data Tree a
  = EmptyTree
  | Tree
      { val :: a,
        left :: Tree a,
        right :: Tree a
      }
  deriving (Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ EmptyTree = mempty
  foldMap f (Tree val l r) =
    foldMap f l
      `mappend` f val
      `mappend` foldMap f r
