module Scrabble where

import Data.Map qualified as M

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score x) = x

instance Semigroup Score where
  (<>) :: Score -> Score -> Score
  (<>) = (+)

instance Monoid Score where
  mempty :: Score
  mempty = Score 0
  mappend :: Score -> Score -> Score
  mappend = (<>)

onePoint :: [Char]
onePoint = ['A', 'a', 'E', 'e', 'I', 'i', 'L', 'l', 'N', 'n', 'O', 'o', 'R', 'r', 'S', 's', 'T', 't', 'U', 'u']

twoPoint :: [Char]
twoPoint = ['D', 'd', 'G', 'g']

threePoint :: [Char]
threePoint = ['C', 'c', 'M', 'm', 'P', 'p']

fourPoint :: [Char]
fourPoint = ['F', 'f', 'v', 'V', 'y', 'Y', 'W', 'w', 'H', 'h']

fivePoint :: [Char]
fivePoint = ['K', 'k']

eightPoint :: [Char]
eightPoint = ['X', 'x', 'J', 'j']

tenPoint :: [Char]
tenPoint = ['Z', 'z', 'Q', 'q']

points :: M.Map Char Int
points =
  M.fromList $
    mconcat
      [ zip onePoint [1, 1 ..],
        zip twoPoint [2, 2 ..],
        zip threePoint [3, 3 ..],
        zip fourPoint [4, 4 ..],
        zip fivePoint [5, 5 ..],
        zip eightPoint [8, 8 ..],
        zip tenPoint [10, 10 ..]
      ]

getPoints :: Maybe Int -> Int
getPoints Nothing = 0
getPoints (Just x) = x

score :: Char -> Score
score k = Score $ getPoints $ M.lookup k points

scoreString :: String -> Score
scoreString = mconcat . map score