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

data Decl
  = ClassDecl
      { classDeclName :: Identifier,
        classDeclParams :: [Identifier],
        classDeclBody :: [Function]
      }
  | FunctionDecl Function
  | VariableDecl
      { variableDeclName :: Identifier,
        variableDeclInitialiser :: Maybe Expression
      }
  | StatementDecl Stmt
  deriving (Eq, Show)

newtype Identifier = Identifier Text
  deriving (Eq, Show)

data Function = Function
  { functionName :: Identifier,
    functionParameters :: [Identifier],
    functionBlock :: Block
  }
  deriving (Eq, Show)

data Stmt
  = ExprStmt Expression
  | ForStmt
      { forStmtInit :: Maybe LoopInitialiser,
        forStmtLoopCond :: Maybe Expression,
        forStmtIncr :: Maybe Expression
      }
  | IfStmt
      { ifStmtCond :: Expression,
        ifStmtContents :: Stmt,
        ifStmtElse :: Maybe Stmt
      }
  | PrintStmt Expression
  | ReturnStmt (Maybe Expression)
  | WhileStmt
      { whileStmtCond :: Expression,
        whileStmtContents :: Stmt
      }
  | BlockStmt Block
  deriving (Eq, Show)

data LoopInitialiser = LoopInitVarDecl Decl | LoopInitExpr Stmt
  deriving (Eq, Show)

newtype Block = Block [Decl]
  deriving (Eq, Show)

newtype Expression = Expression Assignment
  deriving (Eq, Show)

data Assignment
  = Assignment
      { assignmentCall :: Maybe Call,
        assignmentTarget :: Identifier,
        assignmentExpr :: Assignment
      }
  | AssignmentLogicOr LogicOr
  deriving (Eq, Show)

data LogicOr = LogicOr LogicAnd [LogicAnd]
  deriving (Eq, Show)

data LogicAnd = LogicAnd Equality [Equality]
  deriving (Eq, Show)

data Equality = Equality Comparison [(EqualityOperator, Comparison)]
  deriving (Eq, Show)

data EqualityOperator = EqualsOp | NotEqualsOp
  deriving (Eq, Show)

data Comparison = Comparison Term [(ComparisonOperator, Term)]
  deriving (Eq, Show)

data ComparisonOperator = GreaterOp | GreaterEqOp | LessOp | LessEqOp
  deriving (Eq, Show)

data Term = Term Factor [(TermOperator, Factor)]
  deriving (Eq, Show)

data TermOperator = AddOp | SubtractOp
  deriving (Eq, Show)

data Factor = Factor Unary [(FactorOperation, Unary)]
  deriving (Eq, Show)

data FactorOperation = DivideOp | MultiplyOp
  deriving (Eq, Show)

data Unary = Unary UnaryOperator Unary | UnaryCall Call
  deriving (Eq, Show)

data UnaryOperator = NotOp | NegateOp
  deriving (Eq, Show)

data Call = CallFun Primary (Maybe Arguments) | CallProp Primary Identifier
  deriving (Eq, Show)

newtype Arguments = Arguments [Expression]
  deriving (Eq, Show)

data Primary
  = TrueLiteral
  | FalseLiteral
  | NilLiteral
  | ThisLiteral
  | NumberLiteral Double
  | StringLiteral Text
  | IdenLiteral Identifier
  | BracketedExpression Expression
  | SuperDot Identifier
  deriving (Eq, Show)

data Keyword
  = KeywordAnd
  | KeywordClass
  | KeywordElse
  | KeywordFor
  | KeywordFun
  | KeywordIf
  | KeywordOr
  | KeywordPrint
  | KeywordReturn
  | KeywordSuper
  | KeywordVar
  | KeywordWhile
  deriving (Eq, Show)

type Parser = Parsec Void Text

consumeWhitespace :: Parser ()
consumeWhitespace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeWhitespace

--symbol :: Parser a -> Parser a
symbol = L.symbol consumeWhitespace

pBoolLit :: Parser Primary
pBoolLit = do
  x <- string "true" <|> string "false"
  return
    if x == "true"
      then TrueLiteral
      else FalseLiteral

pNumberLit :: Parser Primary
pNumberLit = lexeme do
  f <- try L.float <|> L.decimal
  return $ NumberLiteral f

pIden :: Parser Identifier
pIden = do
  x <- letterChar <|> char '_'
  xs <- many (alphaNumChar <|> char '_')
  let wd = x : xs
  Identifier . pack <$> check wd
  where
    check x =
      if x `elem` kwds
        then fail $ "Keyword '" ++ show x ++ "' cannot be an identifier."
        else return x
    kwds =
      [ "and",
        "class",
        "else",
        "false",
        "for",
        "fun",
        "if",
        "nil",
        "or",
        "print",
        "return",
        "super",
        "this",
        "true",
        "var",
        "while"
      ]

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme $ string keyword <* notFollowedBy alphaNumChar

pStringLit :: Parser Primary
pStringLit = lexeme do
  _ <- char '"'
  str <- manyTill L.charLiteral (char '"')
  return $ StringLiteral $ pack str

pKeywordIden =
  KeywordAnd <$ pKeyword "and"
    <|> KeywordClass <$ pKeyword "class"
    <|> KeywordElse <$ pKeyword "else"
    <|> KeywordFor <$ pKeyword "false"
    <|> KeywordFun <$ pKeyword "fun"
    <|> KeywordIf <$ pKeyword "if"
    <|> KeywordOr <$ pKeyword "or"
    <|> KeywordPrint <$ pKeyword "print"

pPrimary =
  pBoolLit -- TrueLiteral, FalseLiteral
    <|> NilLiteral <$ pKeyword "nil" -- NilLiteral
    <|> ThisLiteral <$ pKeyword "this" -- ThisLiteral
    <|> pNumberLit -- NumberLiteral
    <|> pStringLit -- StringLiteral
    <|> do
      -- SuperDot
      _ <- string "super"
      _ <- char '.'
      SuperDot <$> pIden
    <|> do
      -- BracketedExpression
      _ <- char '('
      e <- pExpression
      _ <- char ')'
      return $ BracketedExpression e
    <|> IdenLiteral <$> pIden

pCall :: Parser Call
pCall = do
  p <- pPrimary
  m <- optional do
    _ <- char '.'
    pIden
  case m of
    Just iden -> return $ CallProp p iden
    Nothing -> do
      openParen <- optional $ char '('
      case openParen of
        Nothing -> return $ CallFun p Nothing
        Just _ -> do
          args <- pArguments
          _ <- char ')'
          return $ CallFun p if null' args then Nothing else Just args
  where
    null' (Arguments a) = Prelude.null a

pArguments :: Parser Arguments
pArguments = Arguments <$> sepBy pExpression (char ',')

pUnary :: Parser Unary
pUnary = lexeme do
  c <- optional $ char '!' <|> char '-'
  case c of
    Just c -> case c of
      '!' -> Unary NotOp <$> pUnary
      '-' -> Unary NegateOp <$> pUnary
      _ -> error "unreachable"
    Nothing -> UnaryCall <$> pCall

pFactor :: Parser Factor
pFactor = do
  f <- pUnary
  x <- many do
    c <- (DivideOp <$ char '/') <|> (MultiplyOp <$ char '*')
    f <- pUnary
    return (c, f)
  return $ Factor f x

pTerm :: Parser Term
pTerm = do
  f <- pFactor
  x <- many do
    c <- (AddOp <$ char '+') <|> (SubtractOp <$ char '-')
    f <- pFactor
    return (c, f)
  return $ Term f x

pComparison :: Parser Comparison
pComparison = do
  f <- pTerm
  x <- many do
    c <-
      GreaterOp <$ char '>'
        <|> GreaterEqOp <$ (char '>' >> char '=')
        <|> LessOp <$ char '>'
        <|> LessEqOp <$ (char '<' >> char '=')
    f <- pTerm
    return (c, f)
  return $ Comparison f x

pEquality :: Parser Equality
pEquality = do
  f <- pComparison
  x <- many do
    c <- (EqualsOp <$ symbol "==") <|> (NotEqualsOp <$ symbol "!=")
    f <- pComparison
    return (c, f)
  return $ Equality f x

pLogicAnd :: Parser LogicAnd
pLogicAnd = do
  f <- pEquality
  x <- many do
    c <- pKeyword "and"
    pEquality
  return $ LogicAnd f x

pLogicOr :: Parser LogicOr
pLogicOr = do
  f <- pLogicAnd
  x <- many do
    c <- pKeyword "or"
    pLogicAnd
  return $ LogicOr f x

pAssignment :: Parser Assignment
pAssignment =
  try do
    assignmentCall <- optional do
      c <- pCall
      _ <- char '.'
      return c
    assignmentTarget <- pIden
    _ <- char '='
    assignmentExpr <- pAssignment
    return Assignment {..}
    <|> AssignmentLogicOr <$> pLogicOr

pExpression :: Parser Expression
pExpression = Expression <$> pAssignment
