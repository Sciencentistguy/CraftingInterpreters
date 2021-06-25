module Instruction where

import Value

data Instruction
  = AddInstr
  | SubInstr
  | NegateInstr
  | ConstantInstr Value
  | PrintInstr
  | MultiplyInstr
  | DivideInstr
  | NotInstr
  | GreaterInstr
  | GreaterEqInstr
  | LessInstr
  | LessEqInstr
  | EqInstr
  | AndInstr
  | OrInstr
  | PopInstr
  | DefineVariableInstr StringType
  | GetVariableInstr StringType
  | SetVariableInstr StringType
  | BeginScopeInstr
  | EndScopeInstr
  | JumpIfFalseInstr Int
  | JumpInstr Int
  | DefineFunctionInstr
  | CallInstr
  | ReturnInstr
  deriving (Show)
