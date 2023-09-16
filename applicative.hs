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
