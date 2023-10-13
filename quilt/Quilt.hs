-- CSCI 360, Fall 2016
-- Project 3: the Quilt language
{-# LANGUAGE GADTs #-}

module Quilt where

import qualified Data.Functor.Identity as I
import Parsing2
import Text.Parsec (ParseError)
import Text.Parsec.Expr
import qualified Text.Parsec.Prim as P
import Text.Parsec.Token (reservedOp, symbol)

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
  X :: Coord
  Y :: Coord
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
  LitColorTrip :: QExpr -> QExpr -> QExpr -> QExpr
  If :: QExpr -> QExpr -> QExpr -> QExpr
  Unary :: UOp -> QExpr -> QExpr
  Arith :: ArithOp -> QExpr -> QExpr -> QExpr
  Comp :: CompOp -> QExpr -> QExpr -> QExpr
  Bool :: BoolOp -> QExpr -> QExpr -> QExpr
  Quilt :: QExpr -> QExpr -> QExpr -> QExpr -> QExpr
  deriving (Show)

lexer :: TokenParser u
lexer =
  makeTokenParser
    emptyDef
      { opStart = oneOf "-+/*=<>&|!",
        opLetter = oneOf "-+/*=<>&|!]",
        reservedOpNames = ["-", "+", "/", "*", "==", "<", ">", "<=", ">=", "&&", "||", "!"],
        reservedNames =
          [ "True",
            "False",
            "if",
            "then",
            "else",
            "red",
            "orange",
            "yellow",
            "green",
            "blue",
            "indigo",
            "purple",
            "violet",
            "quilt",
            "x",
            "y"
          ]
      }

parens :: Parser a -> Parser a
parens = getParens lexer

number :: Parser (Either Integer Double)
number = getNaturalOrFloat lexer

operator :: String -> Parser ()
operator = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () I.Identity a
binary name fun = Infix (do operator name; return fun)

prefix :: String -> (a -> a) -> Operator String () I.Identity a
prefix name fun = Prefix (do operator name; return fun)

-- Parse Color Literals
getColor :: String -> ColorLit
getColor "red" = Red
getColor "orange" = Orange
getColor "yellow" = Yellow
getColor "green" = Green
getColor "blue" = Blue
getColor "indigo" = Indigo
getColor "violet" = Violet
getColor "purple" = Violet
getColor _ = error "Invalid color"

color :: String -> Parser QExpr
color c = reserved c >> return (LitColor (getColor c))

parseColor :: Parser QExpr
parseColor =
  color "red"
    <|> color "orange"
    <|> color "yellow"
    <|> color "green"
    <|> color "blue"
    <|> color "inidigo"
    <|> color "purple"
    <|> color "violet"

-- Parse Numbers
parseNumber :: Parser QExpr
parseNumber = do
  x <- number
  case x of
    Left i -> return $ LitNum $ I i
    Right d -> return $ LitNum $ D d

-- Parse coordinate
x :: Parser QExpr
x = reserved "x" >> return (LitCoord X)

y :: Parser QExpr
y = reserved "y" >> return (LitCoord Y)

parseCoord :: Parser QExpr
parseCoord = x <|> y

-- Parse Boolean
parseBool :: Parser QExpr
parseBool = (reserved "True" >> return (LitBool T)) <|> (reserved "False" >> return (LitBool F))

-- Parse Color Triple
parseColorTriple :: Parser QExpr
parseColorTriple = do
  symbol lexer "["
  a <- qexpr
  whiteSpace
  symbol lexer ","
  b <- qexpr
  whiteSpace
  symbol lexer ","
  c <- qexpr
  whiteSpace
  symbol lexer "]"
  return $ LitColorTrip a b c

parseIf :: Parser QExpr
parseIf = do
  reserved "if"
  whiteSpace
  test <- qexpr
  whiteSpace
  reserved "then"
  whiteSpace
  a <- qexpr
  whiteSpace
  reserved "else"
  whiteSpace
  b <- qexpr
  whiteSpace
  return $ If test a b

parseQuilt :: Parser QExpr
parseQuilt = do
  reserved "quilt"
  whiteSpace
  a <- qexpr
  whiteSpace
  b <- qexpr
  whiteSpace
  c <- qexpr
  whiteSpace
  d <- qexpr
  whiteSpace
  return $ Quilt a b c d

parseQExprAtom :: Parser QExpr
parseQExprAtom = parseColor <|> parseNumber <|> parseBool <|> parseCoord

term :: P.ParsecT String () I.Identity QExpr
term = parens qexpr <|> parseQExprAtom <|> parseColorTriple <|> parseIf <|> parseQuilt

table :: [[Operator String () I.Identity QExpr]]
table =
  [ [prefix "-" (Unary Neg), prefix "!" (Unary Not)],
    [binary "*" (Arith Times) AssocLeft, binary "/" (Arith Divide) AssocLeft],
    [binary "+" (Arith Plus) AssocLeft, binary "-" (Arith Minus) AssocLeft],
    [ binary "==" (Comp Equal) AssocNone,
      binary "<" (Comp Less) AssocNone,
      binary "<=" (Comp LessEq) AssocNone,
      binary ">=" (Comp GreaterEq) AssocNone,
      binary ">" (Comp Greater) AssocNone
    ],
    [binary "&&" (Bool And) AssocLeft, binary "||" (Bool Or) AssocLeft]
  ]

qexpr :: P.ParsecT String () I.Identity QExpr
qexpr = buildExpressionParser table term

parseQExpr :: String -> Either ParseError QExpr
parseQExpr = parse $ getWhiteSpace lexer *> qexpr <* eof

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

-- | Right now, this function ignores the input and simply produces a
--   blue image.  Obviously, you should make this function more
--   interesting!
evalQuilt :: String -> Either String QuiltFun
evalQuilt s = Right $ \x y -> [0, 0, 1]
