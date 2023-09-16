{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use void" #-}

module AParser where

import Control.Applicative
import Data.Char
import Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a, c) -> (b, c)
first g (x, y) = (g x, y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = Parser f
    where
      f [] = Nothing
      f xs =
        case runParser p xs of
          Nothing -> Nothing
          Just ps -> Just $ first g ps

-- Just (12, "13") -> Just ((:[12]), "13")
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser f
    where
      f [] = Nothing
      f xs = Just (x, xs)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 = Parser f -- (Parser (b->('a', b))) <*> char "b"
    where
      f [] = Nothing
      f xs = case runParser p1 xs of -- Just (b->('a', b), "bcdef")
        Nothing -> Nothing
        Just (y, ys) -> case runParser p2 ys of -- Just ('a', "bcdef")
          Nothing -> Nothing
          Just (z, zs) -> Just (y z, zs) -- Just ('b')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser f
    where
      f _ = Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser f
    where
      f xs = runParser p1 xs <|> runParser p2 xs

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ -> reverse . (: [x])) <$> posInt <*> char ' ' <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ satisfy isUpper