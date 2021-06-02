{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Monad
import Control.Monad.Combinators
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

data BinaryOp = AddOp | SubtractOp

data Expr = LiteralExpr Literal | BinaryOpExpr BinaryOp Expr Expr

data Stmt = Print Expr

data Literal = NumberLiteral Double | StringLiteral Text | BoolLiteral Bool | NilLiteral deriving (Show)

type Parser = Parsec Void Text

pNilLit :: Parser Literal
pNilLit = do
  _ <- string "nil"
  return NilLiteral

pBoolLit :: Parser Literal
pBoolLit = do
  x <- string "true" <|> string "false"
  return $ BoolLiteral $ x == "true"

pNumberLit :: Parser Literal
pNumberLit = do
  minus <- optional $ char '-'
  f <- try L.float <|> L.decimal
  return $
    NumberLiteral
      if isJust minus
        then negate f
        else f

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeWhitespace

pNL :: Parser Literal
pNL = lexeme (NumberLiteral <$> L.signed (return ()) (try L.float <|> L.decimal))

pStringLit :: Parser Literal
pStringLit = do
  _ <- char '"'
  str <- manyTill L.charLiteral (char '"')
  return $ StringLiteral $ pack str

consumeWhitespace :: Parser ()
consumeWhitespace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
