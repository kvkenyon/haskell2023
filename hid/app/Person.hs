{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Person where

import GHC.Exts (IsString (fromString))
import TextShow

data Person where
  Person :: String -> Maybe Int -> Person
  deriving (Eq)

instance IsString Person where
  fromString :: String -> Person
  fromString name = Person name Nothing

instance TextShow Person where
  showb :: Person -> Builder
  showb (Person name Nothing) = TextShow.fromString name
  showb (Person name (Just age)) = TextShow.fromString name <> " (" <> showb age <> ")"

homer :: Person
homer = "Homer Simpson"

kevin :: Person
kevin = Person "Kevin Kenyon" (Just 34)