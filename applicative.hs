data Box a = EmptyBox | Package a
  deriving (Show, Read)

instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b
  fmap f EmptyBox = EmptyBox
  fmap f (Package a) = Package $ f a

instance Applicative Box where
  pure _ = EmptyBox
  (<*>) :: Box (a -> b) -> Box a -> Box b
  (<*>) (Package f) (Package g) = Package (f g)