{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import AST
import Control.Monad
import Control.Monad.Combinators
import Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

consumeWhitespace :: Parser ()
consumeWhitespace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeWhitespace

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = L.symbol consumeWhitespace

pBoolLit :: Parser Primary
pBoolLit = label "boolean literal" $ lexeme do
  x <- string "true" <|> string "false"
  return
    if x == "true"
      then TrueLiteral
      else FalseLiteral

pNumberLit :: Parser Primary
pNumberLit = label "number" $ lexeme do
  f <- try L.float <|> L.decimal
  return $ NumberLiteral f

identifier :: Parser Identifier
identifier = label "identifier" $ lexeme do
  x <- letterChar <|> char '_'
  xs <- many (alphaNumChar <|> char '_')
  let wd = x : xs
  Identifier . T.pack <$> check wd
  where
    check x =
      if x `elem` kwds
        then fail $ "Keyword " ++ show x ++ " cannot be an identifier."
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

keyword :: Text -> Parser Text
keyword keyword = lexeme $ string keyword <* notFollowedBy alphaNumChar

pStringLit :: Parser Primary
pStringLit = label "string" $ lexeme do
  _ <- char '"'
  str <- manyTill L.charLiteral (char '"')
  return $ StringLiteral $ T.pack str

pPrimary :: Parser Primary
pPrimary =
  pBoolLit -- TrueLiteral, FalseLiteral
    <|> NilLiteral <$ (keyword "nil" <?> "nil literal") -- NilLiteral
    <|> ThisLiteral <$ keyword "this" -- ThisLiteral
    <|> pNumberLit -- NumberLiteral
    <|> pStringLit -- StringLiteral
    <|> do
      -- SuperDot
      _ <- string "super"
      _ <- char '.'
      SuperDot <$> identifier
    <|> do
      -- BracketedExpression
      _ <- symbol "("
      e <- pExpression
      _ <- symbol ")"
      return $ BracketedExpression e
    <|> IdenLiteral <$> identifier

pCall :: Parser Call
pCall = do
  p <- pPrimary
  m <- optional do
    _ <- char '.'
    identifier
  case m of
    Just iden -> return $ CallProp p iden
    Nothing -> do
      openParen <- optional $ symbol "("
      case openParen of
        Nothing -> return $ CallFun p Nothing
        Just _ -> do
          args <- pArguments
          _ <- symbol ")"
          return $ CallFun p (Just args)
  where
    null' (Arguments a) = Prelude.null a

pArguments :: Parser Arguments
pArguments = Arguments <$> sepBy pExpression (symbol ",")

pUnary :: Parser Unary
pUnary = lexeme do
  c <-
    (fmap . fmap) T.head $
      optional $
        symbol "!"
          <|> symbol "-"
  case c of
    Nothing -> UnaryCall <$> pCall
    Just '!' -> Unary NotOp <$> pUnary
    Just '-' -> Unary NegateOp <$> pUnary
    Just _ -> error "unreachable"

pFactor :: Parser Factor
pFactor = do
  f <- pUnary
  x <- many do
    c <-
      DivideOp <$ symbol "/"
        <|> MultiplyOp <$ symbol "*"
    f <- pUnary
    return (c, f)
  return $ Factor f x

pTerm :: Parser Term
pTerm = do
  f <- pFactor
  x <- many do
    c <-
      AddOp <$ symbol "+"
        <|> SubtractOp <$ symbol "-"
    f <- pFactor
    return (c, f)
  return $ Term f x

pComparison :: Parser Comparison
pComparison = do
  f <- pTerm
  x <- many do
    c <-
      GreaterOp <$ symbol ">"
        <|> GreaterEqOp <$ symbol ">="
        <|> LessOp <$ symbol "<"
        <|> LessEqOp <$ symbol "<="
    f <- pTerm
    return (c, f)
  return $ Comparison f x

pEquality :: Parser Equality
pEquality = do
  f <- pComparison
  x <- many do
    c <- EqualsOp <$ symbol "==" <|> NotEqualsOp <$ symbol "!="
    f <- pComparison
    return (c, f)
  return $ Equality f x

pLogicAnd :: Parser LogicAnd
pLogicAnd = do
  f <- pEquality
  x <- many do
    _ <- keyword "and"
    pEquality
  return $ LogicAnd $ f : x

pLogicOr :: Parser LogicOr
pLogicOr = do
  f <- pLogicAnd
  x <- many do
    _ <- keyword "or"
    pLogicAnd
  return $ LogicOr $ f : x

pAssignment :: Parser Assignment
--FIXME 'a.b = c' should parse, but it does not. This is because pCall will consume the entire
-- iden(.iden)*, so the "char '.'" in assignmentCall fails.
pAssignment =
  try do
    assignmentCall <- optional $
      try $ do
        c <- pCall
        _ <- char '.'
        return c
    assignmentTarget <- identifier
    _ <- symbol "="
    assignmentExpr <- pAssignment
    return Assignment {..}
    <|> AssignmentLogicOr <$> pLogicOr

pExpression :: Parser Expression
pExpression = Expression <$> pAssignment

pBlockStatement :: Parser Statement
pBlockStatement = do
  _ <- symbol "{"
  decls <- many pDeclaration
  _ <- symbol "}"
  return $ BlockStatement $ Block decls

pWhileStatement :: Parser Statement
pWhileStatement = do
  _ <- keyword "while"
  _ <- symbol "("
  whileStmtCond <- pExpression
  _ <- symbol ")"
  whileStmtContents <- pStatement
  return WhileStatement {..}

pReturnStatement :: Parser Statement
pReturnStatement = do
  _ <- keyword "return"
  e <- optional pExpression
  _ <- symbol ";"
  return $ ReturnStatement e

pPrintStatement :: Parser Statement
pPrintStatement = do
  _ <- keyword "print"
  e <- pExpression
  _ <- symbol ";"
  return $ PrintStatement e

pIfStatement :: Parser Statement
pIfStatement = do
  _ <- keyword "if"
  _ <- symbol "("
  ifStmtCond <- pExpression
  _ <- symbol ")"
  ifStmtContents <- pStatement
  ifStmtElse <- optional do
    _ <- keyword "else"
    pStatement
  return IfStatement {..}

pForStatement :: Parser Statement
pForStatement = do
  _ <- keyword "for"
  _ <- symbol "("
  forStmtInit <-
    Just . LoopInitVarDeclaration <$> pVariableDecl
      <|> Just . LoopInitExpr <$> pExpressionStatement
      <|> Nothing <$ symbol ";"
  forStmtLoopCond <- optional pExpression
  _ <- symbol ";"
  forStmtIncr <- optional pExpression
  _ <- symbol ")"
  forStmtContents <- pStatement
  return ForStatement {..}

pExpressionStatement :: Parser Statement
pExpressionStatement = do
  e <- pExpression
  _ <- symbol ";"
  return $ ExpressionStatement e

pStatement :: Parser Statement
pStatement =
  label "statement" $
    pForStatement
      <|> pIfStatement
      <|> pPrintStatement
      <|> pReturnStatement
      <|> pWhileStatement
      <|> pBlockStatement
      <|> pExpressionStatement

pVariableDecl :: Parser Declaration
pVariableDecl = do
  _ <- keyword "var"
  variableDeclName <- identifier
  variableDeclInitialiser <- optional do
    _ <- symbol "="
    pExpression
  _ <- symbol ";"
  return VariableDeclaration {..}

pFunction :: Parser Function
pFunction = do
  functionName <- identifier <?> "function name"
  _ <- symbol "("
  functionParameters <- sepBy identifier (symbol ",")
  _ <- symbol ")"
  functionBlock <- pBlockStatement
  return Function {..}

pFunctionDeclaration :: Parser Declaration
pFunctionDeclaration = do
  _ <- keyword "fun"
  FunctionDeclaration <$> pFunction

pClassDeclaration :: Parser Declaration
pClassDeclaration = do
  _ <- keyword "class"
  classDeclName <- identifier <?> "class name"
  classDeclSuper <- optional do
    _ <- symbol "<"
    identifier <?> "superclass name"
  _ <- symbol "{"
  classDeclBody <- many pFunction
  _ <- symbol "}"
  return ClassDeclaration {..}

pDeclaration :: Parser Declaration
pDeclaration =
  pVariableDecl
    <|> pClassDeclaration
    <|> pFunctionDeclaration
    <|> StatementDeclaration <$> pStatement

pLoxProgram :: Parser LoxProgram
pLoxProgram = do
  decls <- many pDeclaration
  _ <- hidden eof
  return $ LoxProgram decls
