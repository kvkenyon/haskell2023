{-# LANGUAGE BlockArguments #-}

module Parser where

import Control.Applicative (liftA3)
import Control.Monad (liftM)
import Parsing2
import Syntax
import qualified Text.Parsec as P
import Text.Parsec.Token (GenTokenParser (whiteSpace))

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

number :: Parser (Either Integer Double)
number = getNaturalOrFloat lexer

operator :: String -> Parser ()
operator = getReservedOp lexer

binary name fun = Infix (do operator name; return fun)

prefix name fun = Prefix (do operator name; return fun)
