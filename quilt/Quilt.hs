-- CSCI 360, Fall 2016
-- Project 3: the Quilt language
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Quilt where

import qualified Data.Functor.Identity as I
import qualified Data.Map as M
import Parsing2
import Text.Parsec (ParseError)
import Text.Parsec.Expr
import qualified Text.Parsec.Prim as P
import Text.Parsec.Token (reservedOp, symbol)

-- Syntax

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

-- Parser

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
  a <- parseQExpr
  whiteSpace
  symbol lexer ","
  b <- parseQExpr
  whiteSpace
  symbol lexer ","
  c <- parseQExpr
  whiteSpace
  symbol lexer "]"
  return $ LitColorTrip a b c

parseIf :: Parser QExpr
parseIf = do
  reserved "if"
  whiteSpace
  test <- parseQExpr
  whiteSpace
  reserved "then"
  whiteSpace
  a <- parseQExpr
  whiteSpace
  reserved "else"
  whiteSpace
  b <- parseQExpr
  whiteSpace
  return $ If test a b

parseQuilt :: Parser QExpr
parseQuilt = do
  reserved "quilt"
  whiteSpace
  a <- parseQExpr
  whiteSpace
  b <- parseQExpr
  whiteSpace
  c <- parseQExpr
  whiteSpace
  d <- parseQExpr
  whiteSpace
  return $ Quilt a b c d

parseQExprAtom :: Parser QExpr
parseQExprAtom = parseColor <|> parseNumber <|> parseBool <|> parseCoord

term :: P.ParsecT String () I.Identity QExpr
term = parens parseQExpr <|> parseQExprAtom <|> parseColorTriple <|> parseIf <|> parseQuilt

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

parseQExpr :: P.ParsecT String () I.Identity QExpr
parseQExpr = buildExpressionParser table term

qexpr :: String -> Either ParseError QExpr
qexpr = parse $ getWhiteSpace lexer *> parseQExpr <* eof

-- Interpreter

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

interpColor :: ColorLit -> Color
interpColor Red = [1, 0, 0]
interpColor Orange = [1, 165 / 255, 0]
interpColor Yellow = [1, 1, 0]
interpColor Green = [0, 128 / 255, 0]
interpColor Blue = [0, 0, 1]
interpColor Indigo = [75 / 255, 0, 130 / 255]
interpColor Violet = [128 / 255, 0, 128 / 255]

type Env =
  ((Double, Double), (Double, Double), (Double, Double))

interpQuilt :: Env -> QExpr -> QuiltFun
interpQuilt _ (LitColor color) _ _ = interpColor color
interpQuilt _ (LitNum (I i)) _ _ = [fromIntegral i, fromIntegral i, fromIntegral i]
interpQuilt _ (LitNum (D d)) _ _ = [d, d, d]
interpQuilt _ (LitCoord X) x _ = [x, x, x]
interpQuilt _ (LitCoord Y) _ y = [y, y, y]
interpQuilt _ (LitBool T) _ _ = [1, 1, 1]
interpQuilt _ (LitBool F) _ _ = [0, 0, 0]
interpQuilt o (LitColorTrip e1 e2 e3) x y = [i, j, k]
  where
    (i : _) = interpQuilt o e1 x y
    (j : _) = interpQuilt o e2 x y
    (k : _) = interpQuilt o e3 x y
interpQuilt o (If test a b) x y = if pred == 1 then interpQuilt o a x y else interpQuilt o b x y
  where
    (pred : _) = interpQuilt o test x y
interpQuilt o (Unary Neg expr) x y = map negate $ interpQuilt o expr x y
interpQuilt o (Unary Not expr) x y = map mapFn $ interpQuilt o expr x y
  where
    mapFn a = if a == 1 then 0 else 1
interpQuilt o (Arith Plus expr1 expr2) x y = vectorOp Plus a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Arith Minus expr1 expr2) x y = vectorOp Minus a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Arith Times expr1 expr2) x y = vectorOp Times a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Arith Divide expr1 expr2) x y = vectorOp Divide a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Comp Less expr1 expr2) x y = compOp Less a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Comp Greater expr1 expr2) x y = compOp Greater a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Comp GreaterEq expr1 expr2) x y = compOp GreaterEq a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Comp LessEq expr1 expr2) x y = compOp LessEq a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Comp Equal expr1 expr2) x y = compOp Equal a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Bool And expr1 expr2) x y = boolOp And a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt o (Bool Or expr1 expr2) x y = boolOp Or a b
  where
    a = interpQuilt o expr1 x y
    b = interpQuilt o expr2 x y
interpQuilt e@((ox, oy), (l, r), (u, lw)) (Quilt expr1 expr2 expr3 expr4) x y = quilt e x y a b c d
  where
    a = interpQuilt ((l / 2, u / 2), (l, ox), (u, oy)) expr1 x y
    b = interpQuilt ((r / 2, u / 2), (ox, r), (u, oy)) expr2 x y
    c = interpQuilt ((l / 2, lw / 2), (l, ox), (oy, lw)) expr3 x y
    d = interpQuilt ((r / 2, lw / 2), (ox, r), (oy, lw)) expr4 x y

inQuilt :: Env -> Double -> Double -> Bool
inQuilt (_, (left, right), (upper, lower)) x y =
  x >= left && x <= right && y >= lower && y <= upper

quilt :: Env -> Double -> Double -> Color -> Color -> Color -> Color -> Color
quilt e@((ox, oy), (left, right), (upper, lower)) x y a b c d
  | inQuilt e x y && x >= ox = if y >= oy then b else d
  | inQuilt e x y && y >= oy = a
  | inQuilt e x y = c
  | otherwise = []

vectorOp :: ArithOp -> Color -> Color -> Color
vectorOp Plus [x, y, z] [x', y', z'] = [x + x', y + y', z + z']
vectorOp Minus [x, y, z] [x', y', z'] = [x - x', y - y', z - z']
vectorOp Times [x, y, z] [x', y', z'] = [x * x', y * y', z * z']
vectorOp Divide [x, y, z] [x', y', z'] = [x / x', y / y', z / z']

bool2Double :: (Ord a) => (a -> a -> Bool) -> a -> a -> Double
bool2Double op x x' = if x `op` x' then 1 else 0

compOp :: CompOp -> Color -> Color -> Color
compOp Less [x, y, z] [x', y', z'] = [bool2Double (<) x x', bool2Double (<) y y', bool2Double (<) z z']
compOp Greater [x, y, z] [x', y', z'] = [bool2Double (>) x x', bool2Double (>) y y', bool2Double (>) z z']
compOp Equal [x, y, z] [x', y', z'] = [bool2Double (==) x x', bool2Double (==) y y', bool2Double (==) z z']
compOp GreaterEq [x, y, z] [x', y', z'] = [bool2Double (>=) x x', bool2Double (>=) y y', bool2Double (>=) z z']
compOp LessEq [x, y, z] [x', y', z'] = [bool2Double (<=) x x', bool2Double (<=) y y', bool2Double (<=) z z']

boolOp :: BoolOp -> Color -> Color -> Color
boolOp And [x, y, z] [x', _, _] = [bool2Double (&&) a b, y, z]
  where
    a = x == 1
    b = x' == 1
boolOp Or [x, y, z] [x', _, _] = [bool2Double (||) a b, y, z]
  where
    a = x == 1
    b = x' == 1

-- | Type Checker
data Type = TNumeric | TBool | TColor | TCoord
  deriving (Show, Eq)

data TypeError where
  -- Expr w/ error, Expected, Actual
  TypeError :: QExpr -> Type -> Type -> TypeError

instance Show TypeError where
  show :: TypeError -> String
  show (TypeError _ exp act) = "Type Error: In <expr> expected type " ++ show exp ++ " but got type " ++ show act ++ "."

inColorNumeric :: Type -> QExpr -> Either TypeError ()
inColorNumeric TNumeric _ = Right ()
inColorNumeric TColor _ = Right ()
inColorNumeric ty q = Left $ TypeError q TColor ty

infer :: QExpr -> Either TypeError Type
infer (LitColor _) = Right TColor
infer (LitNum _) = Right TNumeric
infer (LitCoord _) = Right TNumeric
infer (LitBool _) = Right TBool
infer (LitColorTrip q1 q2 q3) =
  check q1 TNumeric
    >> check q2 TNumeric
    >> check q3 TNumeric
    >> Right TColor
infer (If test q1 q2) =
  check test TBool
    >> infer q1
    >>= (\ty -> check q2 ty >> Right ty)
-- TODO: TypeError should show expect type A or type B but got Type C
infer (Unary Neg q1) =
  infer q1
    >>= ( \ty -> case ty of
            TNumeric -> Right TNumeric
            TColor -> Right TColor
            _ -> Left $ TypeError q1 TNumeric ty
        )
infer (Unary Not q1) = check q1 TBool >> Right TBool
infer (Arith _ q1 q2) = do
  ty1 <- infer q1
  ty2 <- infer q2
  inColorNumeric ty1 q1 >> inColorNumeric ty2 q2 >> Right TNumeric
infer (Comp _ q1 q2) = check q1 TNumeric >> check q2 TNumeric >> Right TBool
infer (Bool _ q1 q2) = check q1 TBool >> check q2 TBool >> Right TBool
infer (Quilt q1 q2 q3 q4) = do
  ty1 <- infer q1
  ty2 <- infer q2
  ty3 <- infer q3
  ty4 <- infer q4
  if ty1 == ty2 && ty3 == ty4 && ty1 == ty3
    then case ty1 of
      TNumeric -> Right TNumeric
      TBool -> Right TBool
      _ -> Right TColor
    else inColorNumeric ty1 q1 >> inColorNumeric ty2 q2 >> inColorNumeric ty3 q3 >> inColorNumeric ty4 q4 >> Right TColor

check :: QExpr -> Type -> Either TypeError ()
check q ty = infer q >>= (\ty2 -> if ty == ty2 then Right () else Left $ TypeError q ty ty2)

-- | Parse the string into a QExpr or produce a ParseError
--   On success we interpret the AST into a Quilt Function
evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case qexpr s of
  Left err -> Left $ show err
  Right ast -> case infer ast of
    Left tyErr -> Left $ show tyErr
    Right ty -> Right $ interpQuilt ((0, 0), (-1, 1), (1, -1)) ast
