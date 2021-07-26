module Compiler
  ( compile,
  )
where

import AST
import Data.Maybe
import Types

compile :: LoxProgram -> [Instruction]
compile = cLoxProgram -- TODO make sure this is in the right order

cLoxProgram :: LoxProgram -> [Instruction]
cLoxProgram (LoxProgram decls) = go decls []
  where
    go [] acc = acc
    go (x : xs) acc = go xs $ acc ++ cDecl x

cDecl :: Declaration -> [Instruction]
cDecl decl = case decl of
  ClassDeclaration {..} ->
    let name = unwrapIdentifier classDeclName
     in [DefineClassInstr name, DefineVariableInstr name]
  FunctionDeclaration func -> cFunction func
  VariableDeclaration {..} -> cVariableDecl variableDeclName variableDeclInitialiser
  StatementDeclaration stmt -> cStatement stmt

cFunction :: Function -> [Instruction]
cFunction Function {..} =
  let Identifier name = functionName
      function = LoxFunction Nothing (length functionParameters) name
      collectArgs = reverse $ map (\(Identifier x) -> DefineVariableInstr x) functionParameters
      content = cStatement functionBlock
   in [ ConstantInstr $ FunctionValue function,
        DefineFunctionInstr,
        DefineVariableInstr name
      ]
        ++ [JumpInstr $length collectArgs + length content + 3]
        ++ [BeginScopeInstr]
        ++ collectArgs
        ++ content
        ++ [ConstantInstr NilValue, ReturnInstr]

cVariableDecl :: Identifier -> Maybe Expression -> [Instruction]
cVariableDecl (Identifier name) expr =
  let val = maybe [ConstantInstr NilValue] cExpression expr
   in val ++ [DefineVariableInstr name]

cStatement :: Statement -> [Instruction]
cStatement stmt = case stmt of
  ExpressionStatement expr -> cExpression expr ++ [PopInstr]
  ForStatement {..} ->
    -- TODO possible optimisation: if condition is Nothing, then omit JumpIfFalseInstr entirely
    let init = case forStmtInit of
          Nothing -> []
          Just (LoopInitExpr stmt) -> cStatement stmt
          Just (LoopInitVarDeclaration decl) -> cDecl decl
        condition = maybe [ConstantInstr $ BooleanValue True] cExpression forStmtLoopCond
        incr = maybe [] cExpression forStmtIncr
        contents = cStatement forStmtContents
        forwardJumpLength = 1 + length contents + length condition + length incr
     in BeginScopeInstr :
        init
          ++ condition
          ++ [JumpIfFalseInstr forwardJumpLength]
          ++ contents
          ++ incr
          ++ [JumpInstr $ negate $ forwardJumpLength + 1]
  IfStatement {..} ->
    let contents = cStatement ifStmtContents
     in cExpression ifStmtCond
          ++ [JumpIfFalseInstr $ length contents + if isJust ifStmtElse then 1 else 0]
          ++ contents
          ++ case ifStmtElse of
            Nothing -> []
            Just else' ->
              let contents = cStatement else'
               in JumpInstr (length contents) : contents
  PrintStatement expr -> cExpression expr ++ [PrintInstr]
  ReturnStatement maybe_expr ->
    maybe
      [ConstantInstr NilValue]
      cExpression
      maybe_expr
      ++ [ReturnInstr]
  WhileStatement {..} ->
    let contents = cStatement whileStmtContents
        output =
          cExpression whileStmtCond
            ++ [JumpIfFalseInstr $ length contents + 1] -- +1 because of the JumpInstr
            ++ contents
     in output ++ [JumpInstr (negate $ length output + 1)] -- +1 because it has to jump over itself as well
  BlockStatement (Block decls) -> [BeginScopeInstr] ++ concatMap cDecl decls ++ [EndScopeInstr]

cExpression :: Expression -> [Instruction]
cExpression (Expression assign) = cAssignment assign

cAssignment :: Assignment -> [Instruction]
cAssignment assign = case assign of
  Assignment call assignmentExpr ->
    cAssignment assignmentExpr ++ case call of
      CallFun (IdenLiteral (Identifier name)) _ -> [SetVariableInstr name]
      CallProp inst (Identifier prop) -> cPrimary inst ++ [SetPropInstr prop]
      _ -> undefined
  AssignmentLogicOr logicor -> cLogicOr logicor

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
    Just (Arguments args) -> cPrimary primary ++ concatMap cExpression args ++ [CallInstr]
  CallProp primary (Identifier propName) -> cPrimary primary ++ [GetPropInstr propName]

cPrimary :: Primary -> [Instruction]
cPrimary primary = case primary of
  TrueLiteral -> [ConstantInstr $ BooleanValue True]
  FalseLiteral -> [ConstantInstr $ BooleanValue False]
  NilLiteral -> [ConstantInstr NilValue]
  ThisLiteral -> undefined
  NumberLiteral num -> [ConstantInstr $ NumberValue num]
  StringLiteral str -> [ConstantInstr $ StringValue str]
  IdenLiteral (Identifier iden) -> [GetVariableInstr iden]
  BracketedExpression expr -> cExpression expr
  SuperDot iden -> undefined
