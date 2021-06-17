{-# LANGUAGE RecordWildCards #-}

module Compiler
  ( compile,
  )
where

import AST
import Data.Maybe
import Data.Text (Text)
import Instructions

compile :: LoxProgram -> [Instruction]
compile = cLoxProgram -- TODO make sure this is in the right order

cLoxProgram :: LoxProgram -> [Instruction]
cLoxProgram (LoxProgram decls) = go decls []
  where
    go [] acc = acc
    go (x : xs) acc = go xs $ acc ++ cDecl x

cDecl :: Declaration -> [Instruction]
cDecl decl = case decl of
  ClassDeclaration {..} -> undefined
  FunctionDeclaration func -> undefined
  VariableDeclaration {..} -> cVariableDecl variableDeclName variableDeclInitialiser
  StatementDeclaration stmt -> cStatement stmt

cVariableDecl :: Identifier -> Maybe Expression -> [Instruction]
cVariableDecl (Identifier name) expr =
  let val = case expr of
        Just a -> cExpression a
        Nothing -> [ConstantInstr NilValue]
   in val ++ [DefineGlobalInstr name]

cStatement :: Statement -> [Instruction]
cStatement stmt = case stmt of
  ExpressionStatement expr -> cExpression expr ++ [ReturnInstr]
  ForStatement {..} -> undefined
  IfStatement {..} -> undefined
  PrintStatement expr -> cExpression expr ++ [PrintInstr]
  ReturnStatement maybe_expr -> undefined
  WhileStatement {..} -> undefined
  BlockStatement block -> undefined

cExpression :: Expression -> [Instruction]
cExpression (Expression assign) = cAssignment assign

cAssignment :: Assignment -> [Instruction]
cAssignment assign = case assign of
  Assignment _ (Identifier name) assignmentExpr ->
    cAssignment assignmentExpr ++ [SetGlobalInstr name]
  AssignmentLogicOr logicor -> cLogicOr logicor

--assignmentCall :: Maybe Call,
--assignmentTarget :: Identifier,
--assignmentExpr :: Assignment

cLogicOr :: LogicOr -> [Instruction]
cLogicOr (LogicOr (and : rest)) = cLogicAnd and ++ go rest []
  where
    go [] acc = acc
    go (x : xs) acc = go xs (cLogicAnd x ++ [OrInstr] ++ acc)
cLogicOr (LogicOr []) = error "unreachable: parser does not allow this"

cLogicAnd :: LogicAnd -> [Instruction]
cLogicAnd (LogicAnd (equality : rest)) = cEquality equality ++ go rest []
  where
    go [] acc = acc
    go (x : xs) acc = go xs (cEquality x ++ [AndInstr] ++ acc)
cLogicAnd (LogicAnd []) = error "unreachable: parser does not allow this"

cEquality :: Equality -> [Instruction]
cEquality (Equality comparison rest) = cComparison comparison ++ go rest []
  where
    go [] acc = acc
    go ((op, comp) : xs) acc = go xs $ case op of
      EqualsOp -> cComparison comp ++ [EqInstr] ++ acc
      NotEqualsOp -> cComparison comp ++ [EqInstr, NotInstr] ++ acc

cComparison :: Comparison -> [Instruction]
cComparison (Comparison term rest) = cTerm term ++ go rest []
  where
    go [] acc = acc
    go ((op, trm) : xs) acc = go xs (cTerm trm ++ [opToInstr op] ++ acc)

    opToInstr GreaterOp = GreaterInstr
    opToInstr GreaterEqOp = GreaterEqInstr
    opToInstr LessOp = LessInstr
    opToInstr LessEqOp = LessEqInstr

cTerm :: Term -> [Instruction]
cTerm (Term factor rest) = cFactor factor ++ go rest []
  where
    go [] acc = acc
    go ((op, fact) : xs) acc =
      go xs (cFactor fact ++ [opToInstr op] ++ acc)

    opToInstr AddOp = AddInstr
    opToInstr SubtractOp = SubInstr

cFactor :: Factor -> [Instruction]
cFactor (Factor unary rest) = cUnary unary ++ go rest []
  where
    go [] acc = acc
    go ((op, unry) : xs) acc = go xs (cUnary unry ++ [opToInstr op] ++ acc)

    opToInstr MultiplyOp = MultiplyInstr
    opToInstr DivideOp = DivideInstr

cUnary :: Unary -> [Instruction]
cUnary unary = case unary of
  Unary op unary -> cUnary unary ++ [opToInstr op]
  UnaryCall call -> cCall call
  where
    opToInstr NegateOp = NegateInstr
    opToInstr NotOp = NotInstr

cCall :: Call -> [Instruction]
cCall call = case call of
  CallFun primary args -> case args of
    Nothing -> cPrimary primary
    Just args -> undefined
  CallProp primary iden -> undefined

cPrimary :: Primary -> [Instruction]
cPrimary primary = case primary of
  TrueLiteral -> [ConstantInstr $ BooleanValue True]
  FalseLiteral -> [ConstantInstr $ BooleanValue False]
  NilLiteral -> [ConstantInstr NilValue]
  ThisLiteral -> undefined
  NumberLiteral num -> [ConstantInstr $ NumberValue num]
  StringLiteral str -> [ConstantInstr $ StringValue str]
  IdenLiteral (Identifier iden) -> [GetGlobalInstr iden]
  BracketedExpression expr -> cExpression expr
  SuperDot iden -> undefined
