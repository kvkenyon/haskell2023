import Control.Applicative

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

type Name = String

data Employee = Employee
  { name :: Name,
    phone :: String
  }

maybeEmployee :: Maybe Name -> Maybe String -> Maybe Employee
maybeEmployee Nothing _ = Nothing
maybeEmployee _ Nothing = Nothing
maybeEmployee (Just name) (Just phone) = Just $ Employee name phone

(*>) :: Applicative f => f a -> f b -> f b
(*>) fa fb = fb

{--

Just 3 and a Just [4] -> Just [3,4]

liftA2 (:) Just 3 Just 4

--}

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA g xs = sequenceA' (fmap g xs)

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = sequenceA' (replicate n fa)