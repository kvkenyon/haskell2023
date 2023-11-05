{-# LANGUAGE InstanceSigs #-}

module Fixed where

import Data.Fixed

data E4

instance HasResolution E4 where
  resolution :: p E4 -> Integer
  resolution _ = 10000

type Fixed4 = Fixed E4

pi :: Fixed4
pi = 3.14159

e :: Fixed4
e = 2.71828