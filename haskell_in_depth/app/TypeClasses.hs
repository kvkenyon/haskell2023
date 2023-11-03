{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module TypeClasses where

import Fmt

deriving instance Ord Turn

data Direction = North | East | South | West
  deriving (Eq, Show, Read, Bounded, Enum, CyclicEnum)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Show, Read, Bounded, Enum)

instance Buildable Direction where
  build :: Direction -> Builder
  build North = "N"
  build South = "S"
  build West = "W"
  build East = "E"

instance Buildable Turn where
  build :: Turn -> Builder
  build TNone = "--"
  build TAround = "||"
  build TLeft = "<-"
  build TRight = "->"

instance Semigroup Turn where
  (<>) :: Turn -> Turn -> Turn
  TNone <> t = t
  TLeft <> TRight = TNone
  TLeft <> TLeft = TAround
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty :: Turn
  mempty = TNone

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

class (Eq a, Bounded a, Enum a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany d ts = rotate (mconcat ts) d

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany d@(_ : _ : _) = zipWith orient (tail d) d
orientMany _ = []

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname =
  do
    f <- readFile fname
    let turns = map read $ lines f
        dirs = rotateManySteps dir turns
        finalDir = rotateMany dir turns
    fmtLn $
      "Final direction: "
        +|| finalDir
        ||+ " "

    fmt $ nameF "Intermediate directions" (unwordsF dirs)

-- orientFromFile :: FilePath -> IO ()
