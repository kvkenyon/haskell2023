-- CSCI 360, Fall 2016
-- Project 3: the Quilt language
{-# LANGUAGE GADTs #-}

module Quilt where

-- | Color Literals represent ROYGBIV values
--   They will be interpreted by triples of floating point numbers (Color)
--   Each will be a constant
data ColorLit where
  Red :: ColorLit
  Orange :: ColorLit
  Yellow :: ColorLit
  Green :: ColorLit
  Blue :: ColorLit
  Indigo :: ColorLit
  Violet :: ColorLit
  deriving (Show)

-- | Single numeric value are either integer or double
data Number where
  D :: Double -> Number
  I :: Integer -> Number
  deriving (Show)

-- | Coordinate values have numbers for each dim
data Coord where
  X :: Number -> Coord
  Y :: Number -> Coord
  deriving (Show)

data Boolean where
  T :: Boolean
  F :: Boolean
  deriving (Show)

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

data UOp where
  Neg :: UOp
  Not :: UOp
  deriving (Show)

data ArithOp where
  Plus :: ArithOp
  Minus :: ArithOp
  Times :: ArithOp
  Divide :: ArithOp
  deriving (Show)

data CompOp where
  Less :: CompOp
  Greater :: CompOp
  Equal :: CompOp
  LessEq :: CompOp
  GreaterEq :: CompOp
  deriving (Show)

data BoolOp where
  And :: BoolOp
  Or :: BoolOp
  deriving (Show)

data QExpr where
  LitColor :: ColorLit -> QExpr
  LitNum :: Number -> QExpr
  LitCoord :: Coord -> QExpr
  LitBool :: Boolean -> QExpr
  LitColorTrip :: Color -> QExpr
  If :: QExpr -> QExpr -> QExpr -> QExpr
  Unary :: UOp -> QExpr -> QExpr
  Arith :: ArithOp -> QExpr -> QExpr -> QExpr
  Comp :: CompOp -> QExpr -> QExpr -> QExpr
  Quilt :: QExpr -> QExpr -> QExpr -> QExpr -> QExpr
  deriving (Show)

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

-- | Right now, this function ignores the input and simply produces a
--   blue image.  Obviously, you should make this function more
--   interesting!
evalQuilt :: String -> Either String QuiltFun
evalQuilt s = Right $ \x y -> [0, 0, 1]
