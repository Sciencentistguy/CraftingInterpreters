module AST where

import Data.Text (Text)

newtype LoxProgram = LoxProgram [Declaration]
  deriving (Show)

data Declaration
  = ClassDeclaration
      { classDeclName :: Identifier,
        classDeclSuper :: Maybe Identifier,
        classDeclBody :: [Function]
      }
  | FunctionDeclaration Function
  | VariableDeclaration
      { variableDeclName :: Identifier,
        variableDeclInitialiser :: Maybe Expression
      }
  | StatementDeclaration Statement
  deriving (Eq, Show)

newtype Identifier = Identifier Text
  deriving (Eq, Show)

data Function = Function
  { functionName :: Identifier,
    functionParameters :: [Identifier],
    functionBlock :: Statement
  }
  deriving (Eq, Show)

data Statement
  = ExpressionStatement Expression
  | ForStatement
      { forStmtInit :: Maybe LoopInitialiser,
        forStmtLoopCond :: Maybe Expression,
        forStmtIncr :: Maybe Expression,
        forStmtContents :: Statement
      }
  | IfStatement
      { ifStmtCond :: Expression,
        ifStmtContents :: Statement,
        ifStmtElse :: Maybe Statement
      }
  | PrintStatement Expression
  | ReturnStatement (Maybe Expression)
  | WhileStatement
      { whileStmtCond :: Expression,
        whileStmtContents :: Statement
      }
  | BlockStatement Block
  deriving (Eq, Show)

data LoopInitialiser = LoopInitVarDeclaration Declaration | LoopInitExpr Statement
  deriving (Eq, Show)

newtype Block = Block [Declaration]
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
