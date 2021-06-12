module Instructions where

import Data.Text (Text)
import qualified Data.Text as T

data Value = NumberValue Double | StringValue Text | NilValue | BooleanValue Bool

instance Show Value where
  show val = case val of
    NumberValue num -> show num
    StringValue s -> T.unpack s
    NilValue -> "nil"
    BooleanValue b -> show b

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
  | ReturnInstr
  deriving (Show)

valueAdd :: Value -> Value -> Either String Value
valueAdd a b = case a of
  NumberValue a ->
    case b of
      NumberValue b -> Right $ NumberValue $ a + b
      _ -> Left "Operands to '+' must be numbers"
  _ -> Left "Operands to '+' must be numbers"

valueSub :: Value -> Value -> Either String Value
valueSub a b = case a of
  NumberValue a ->
    case b of
      NumberValue b -> Right $ NumberValue $ a - b
      _ -> Left "Operands to '-' must be numbers"
  _ -> Left "Operands to '-' must be numbers"

valueMul :: Value -> Value -> Either String Value
valueMul a b = case a of
  NumberValue a ->
    case b of
      NumberValue b -> Right $ NumberValue $ a * b
      _ -> Left "Operands to '*' must be numbers"
  _ -> Left "Operands to '*' must be numbers"

valueDiv :: Value -> Value -> Either String Value
valueDiv a b = case a of
  NumberValue a ->
    case b of
      NumberValue b -> Right $ NumberValue $ a / b
      _ -> Left "Operands to '/' must be numbers"
  _ -> Left "Operands to '/' must be numbers"

valueNegate :: Value -> Either String Value
valueNegate a = case a of
  NumberValue a -> Right $ NumberValue $ - a
  _ -> Left "Operand to unary '-' must be a number"

valueToBool :: Value -> Bool
valueToBool a = case a of
  BooleanValue b -> b
  NilValue -> False
  _ -> True
