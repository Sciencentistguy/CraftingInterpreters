{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Monad.Except
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec.Error

type StringType = Text

data LoxFunction
  = LoxFunction
      { lfLocation :: Maybe Int,
        lfArity :: Int,
        lfName :: StringType
      }
  | NativeFunction
      { nfName :: StringType,
        nfArity :: Int,
        nfFunction :: [Value] -> IOResult Value
      }

data Value
  = NumberValue Double
  | StringValue StringType
  | NilValue
  | BooleanValue Bool
  | FunctionValue LoxFunction

type Result = Either LoxError

type IOResult = ExceptT LoxError IO

data LoxError
  = ParseError (ParseErrorBundle Text Void)
  | NumArgsError Int [Value]
  | TypeMismatchError String String
  | UnboundVariableError String
  | VariableExistsError String
  | GenericError String
  | InternalError String

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

instance Show LoxError where
  show (NumArgsError expected actual) =
    "Error: Incorrect number of arguments. Expected "
      ++ show expected
      ++ ", received `"
      ++ show actual
      ++ "`."
  show (ParseError peb) = errorBundlePretty peb
  show (TypeMismatchError expected actual) =
    "Error: Type mismatch: Expected `"
      ++ expected
      ++ "`, received `"
      ++ actual
      ++ "`."
  show (UnboundVariableError name) = "Error: Variable `" ++ name ++ "` is not bound."
  show (VariableExistsError name) =
    "Error: Variable `"
      ++ name
      ++ "` already exists in this scope."
  show (InternalError msg) = "Internal compiler error: " ++ msg ++ "."
  show (GenericError msg) = "Error:" ++ msg ++ "."

instance Show LoxFunction where
  show LoxFunction {..} = "<fn " ++ T.unpack lfName ++ "(" ++ show lfArity ++ ")>"
  show NativeFunction {..} = "<native fn " ++ T.unpack nfName ++ "(" ++ show nfArity ++ ")>"

instance Show Value where
  show val = case val of
    NumberValue num -> show num
    StringValue s -> show $ T.unpack s
    NilValue -> "nil"
    BooleanValue b -> show b
    FunctionValue f -> show f

valueGetType :: Value -> String
valueGetType v = case v of
  NumberValue _ -> "number"
  StringValue _ -> "string"
  NilValue -> "nil"
  BooleanValue _ -> "boolean"
  FunctionValue _ -> "function"

valueToNumber :: Value -> Result Double
valueToNumber v = case v of
  NumberValue n -> return n
  _ -> throwError $ TypeMismatchError "number" (valueGetType v)

valueToString :: Value -> Result Text
valueToString v = case v of
  StringValue s -> return s
  _ -> throwError $ TypeMismatchError "string" (valueGetType v)

valueCoerceBool :: Value -> Bool
valueCoerceBool v = case v of
  BooleanValue b -> b
  NilValue -> False
  _ -> True

valueToBool :: Value -> Result Bool
valueToBool v = case v of
  BooleanValue b -> return b
  _ -> throwError $ TypeMismatchError "bool" (valueGetType v)

valueToFunction :: Value -> Result LoxFunction
valueToFunction v = case v of
  FunctionValue f -> return f
  _ -> throwError $ TypeMismatchError "function" (valueGetType v)

valueAdd :: Value -> Value -> Result Value
valueAdd a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a + b

valueSub :: Value -> Value -> Result Value
valueSub a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a - b

valueMul :: Value -> Value -> Result Value
valueMul a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a * b

valueDiv :: Value -> Value -> Result Value
valueDiv a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a / b

valueNegate :: Value -> Result Value
valueNegate a = NumberValue . negate <$> valueToNumber a

valueEqual :: Value -> Value -> Result Bool
valueEqual a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ a == b

valueGreater :: Value -> Value -> Result Bool
valueGreater a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ a > b

valueLess :: Value -> Value -> Result Bool
valueLess a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ a < b

liftResult :: (MonadError LoxError m) => Result a -> m a
liftResult (Right v) = return v
liftResult (Left e) = throwError e

runIOResult :: IOResult String -> IO String
runIOResult action =
  runExceptT (action `catchError` (return . show)) <&> \case
    Right x -> x
    Left x -> show x

liftMaybe :: (MonadError LoxError m) => LoxError -> Maybe a -> m a
liftMaybe _ (Just a) = return a
liftMaybe err Nothing = throwError err
