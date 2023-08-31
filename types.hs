module Tizypes where
import Distribution.Simple (VersionInterval)

type FirstName = String
type LastName = String
type PhoneNumber = String
type ZipCode = Int

data Address = Address {
    streetNumber :: Int
    , streetName :: String
    , zipCode :: ZipCode
    , city :: String
    , state :: String
    , country :: String
} deriving (Show, Read, Eq, Ord)

data Person = Person {
    fname :: FirstName,
    lname :: LastName,
    phoneNum :: PhoneNumber,
    address :: Address
} deriving (Show)

data Point = Point Double Double
    deriving (Show)

data Shape = Circle {
    radius :: Double,
    location :: Point
} | Rectangle Point Point Point Point
    deriving (Show)

newtype Map k v = Map [(k,v)]

data Vector a = Vector a a a
    deriving (Show)

vecadd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x y z) `vecadd` (Vector x' y' z') = Vector (x + x') (y + y') (z + z')

vecmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x y z) `vecmult` (Vector x' y' z') = Vector (x*x') (y*y') (z*z')

dot:: (Num a) => Vector a -> Vector a -> a
(Vector x y z) `dot` (Vector x' y' z') = (x*x') + (y*y') + (z*z')


data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)

safeAdd :: (Num a) => Tizypes.Maybe a -> Tizypes.Maybe a -> Tizypes.Maybe a
safeAdd Tizypes.Nothing _ = Tizypes.Nothing
safeAdd _ Tizypes.Nothing = Tizypes.Nothing
safeAdd (Tizypes.Just x) (Tizypes.Just y) = Tizypes.Just $ x + y

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

type AssocList k v = [(k,v)]

type Mayethbe = Tizypes.Maybe

get :: (Eq k) => k -> AssocList k v -> Mayethbe v
get key = foldl (\acc (k', v') -> if k' == key then Tizypes.Just v' else acc) Tizypes.Nothing
