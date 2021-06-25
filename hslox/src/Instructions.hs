{-# LANGUAGE BlockArguments #-}

module Instructions where

import Data.Text (Text)
import qualified Data.Text as T

type StringType = Text

data Value
  = NumberValue Double
  | StringValue StringType
  | NilValue
  | BooleanValue Bool
  | FunctionValue LoxFunction

data LoxFunction = LoxFunction
  { lfLocation :: Maybe Int,
    lfArity :: Int,
    lfName :: StringType
  }
  deriving (Show)

instance Show Value where
  show val = case val of
    NumberValue num -> show num
    StringValue s -> T.unpack s
    NilValue -> "nil"
    BooleanValue b -> show b
    FunctionValue f -> show f

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

errorMsg :: a -> Maybe b -> Either a b
errorMsg _ (Just a) = Right a
errorMsg msg Nothing = Left msg

valueToNumber :: Value -> Maybe Double
valueToNumber v = case v of
  NumberValue a -> Just a
  _ -> Nothing

valueToString :: Value -> Maybe Text
valueToString v = case v of
  StringValue s -> Just s
  _ -> Nothing

valueToBool :: Value -> Bool
valueToBool v = case v of
  BooleanValue b -> b
  NilValue -> False
  _ -> True

valueToFunction :: Value -> Maybe LoxFunction
valueToFunction v = case v of
  FunctionValue v -> Just v
  _ -> Nothing

valueAdd :: Value -> Value -> Either String Value
valueAdd a b = errorMsg "Operands to '+' must be numbers" $
  do
    a <- valueToNumber a
    b <- valueToNumber b
    return $ NumberValue $ a + b

valueSub :: Value -> Value -> Either String Value
valueSub a b = errorMsg "Operands to '-' must be numbers" $
  do
    a <- valueToNumber a
    b <- valueToNumber b
    return $ NumberValue $ a - b

valueMul :: Value -> Value -> Either String Value
valueMul a b = errorMsg "Operands to '*' must be numbers" $
  do
    a <- valueToNumber a
    b <- valueToNumber b
    return $ NumberValue $ a * b

valueDiv :: Value -> Value -> Either String Value
valueDiv a b = errorMsg "Operands to '/' must be numbers" $
  do
    a <- valueToNumber a
    b <- valueToNumber b
    return $ NumberValue $ a / b

valueNegate :: Value -> Either String Value
valueNegate a = errorMsg "Operand to unary '-' must be a number" $ NumberValue . negate <$> valueToNumber a

valueEqual :: Value -> Value -> Either String Bool
valueEqual a b =
  errorMsg "Comparison operands must be the same type" $
    do
      a <- valueToNumber a
      b <- valueToNumber b
      return $ a == b

valueGreater :: Value -> Value -> Either String Bool
valueGreater a b =
  errorMsg "Comparison operands must be the same type" $
    do
      a <- valueToNumber a
      b <- valueToNumber b
      return $ a > b

valueLess :: Value -> Value -> Either String Bool
valueLess a b =
  errorMsg "Comparison operands must be the same type" $
    do
      a <- valueToNumber a
      b <- valueToNumber b
      return $ a < b

--case a of
--NumberValue a -> case b of
--NumberValue b -> Right $ a > b
--_ -> Left "Comparison operands must be the same type"
--_ -> Left "Comparison operands must be the same type"
