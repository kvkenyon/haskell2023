{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.Bifunctor (bimap)
import Data.List (sort)
import GHC.Float (int2Double)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random :: RandomGen g => g -> (DieValue, g)
  random = first DV . randomR (1, 6)
  randomR :: RandomGen g => (DieValue, DieValue) -> g -> (DieValue, g)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
  deriving (Show)

maxAttackers :: Army -> Army
maxAttackers 0 = 0
maxAttackers 1 = 0
maxAttackers 2 = 1
maxAttackers 3 = 2
maxAttackers _ = 3

maxDefenders :: Army -> Army
maxDefenders 0 = 0
maxDefenders 1 = 1
maxDefenders _ = 2

rollArmy :: Army -> Rand StdGen [DieValue]
rollArmy 0 = return []
rollArmy n = do
  roll <- die
  rolls <- rollArmy (n - 1)
  return (roll : rolls)

score :: [(Int, Int)] -> (Int, Int)
score = foldr (\x acc -> bimap (fst acc +) (snd acc +) x) (0, 0)

armyOutcomes :: [DieValue] -> [DieValue] -> [(Int, Int)]
armyOutcomes = zipWith (\a b -> if a <= b then (-1, 0) else (0, -1))

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  let offense = maxAttackers (attackers bf)
  let defense = maxDefenders (defenders bf)

  rollsOffense <- rollArmy offense
  rollsDefense <- rollArmy defense

  let sortedO = reverse $ sort rollsOffense
  let sortedD = reverse $ sort rollsDefense

  let outcomes = armyOutcomes sortedO sortedD

  let outcome = score outcomes

  return (Battlefield (attackers bf + fst outcome) (defenders bf + snd outcome))

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  newBf <- battle bf

  let isEnd = attackers newBf < 2 || (defenders newBf == 0)

  if isEnd then return newBf else invade newBf

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  let wins = length (filter (\r -> attackers r > 2) results)
  return $ int2Double wins / 1000

main :: IO ()
main = do
  bf <- evalRandIO $ successProb $ Battlefield 10 6
  print bf
