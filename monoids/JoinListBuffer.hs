{-# LANGUAGE FlexibleInstances #-}

module JoinListBuffer where

import Buffer
import Data.Monoid
import JoinList
import Scrabble
import Sized

singleBuf :: String -> JoinList (Score, Size) String
singleBuf x = Single (scoreString x, Size 1) x

initBuff :: JoinList (Score, Size) String
initBuff =
  foldl
    (+++)
    Empty
    ( map
        singleBuf
        [ "This buffer is for notes you don't want to save, and for",
          "evaluation of steam valve coefficients.",
          "To load a different file, type the character L followed",
          "by the name of the file."
        ]
    )

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString Empty = []
  toString (Single _ x) = x ++ "\n"
  toString (Append _ l1 l2) = toString l1 ++ toString l2

  fromString :: String -> JoinList (Score, Size) String
  fromString s = foldl (+++) Empty (map singleBuf (lines s))

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
  replaceLine i delta jl = pre +++ new +++ post
    where
      pre = takeJ i jl
      post = dropJ (i + 1) jl
      new = singleBuf delta

  numLines :: JoinList (Score, Size) String -> Int
  numLines jl = getSize $ snd $ tag jl

  value :: JoinList (Score, Size) String -> Int
  value jl = getScore $ fst $ tag jl