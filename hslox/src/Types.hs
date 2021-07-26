{-# LANGUAGE LambdaCase #-}

module Types where

import Control.Monad.Except
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.IORef
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

newtype LoxClass = LoxClass
  { lcName :: StringType
  }

data LoxInstance = LoxInstance
  { liClass :: LoxClass,
    liEnv :: IORef (HashMap StringType (IORef Value))
  }

data Value
  = NumberValue Double
  | StringValue StringType
  | NilValue
  | BooleanValue Bool
  | FunctionValue LoxFunction
  | ClassValue LoxClass
  | InstanceValue LoxInstance

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
  | DefineClassInstr StringType
  | SetPropInstr StringType
  | GetPropInstr StringType
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

instance Show LoxClass where
  show LoxClass {..} = "<class " ++ T.unpack lcName ++ ">"

instance Show LoxInstance where
  show (LoxInstance (LoxClass name) _) = "<instance of " ++ T.unpack name ++ ">"

instance Show Value where
  show val = case val of
    NumberValue num -> show num
    StringValue s -> show $ T.unpack s
    NilValue -> "nil"
    BooleanValue b -> show b
    FunctionValue f -> show f
    ClassValue c -> show c
    InstanceValue i -> show i

valueGetType :: Value -> String
valueGetType v = case v of
  NumberValue _ -> "number"
  StringValue _ -> "string"
  NilValue -> "nil"
  BooleanValue _ -> "boolean"
  FunctionValue _ -> "function"
  ClassValue _ -> "class"
  InstanceValue _ -> "instance"

valueToNumber :: MonadError LoxError m => Value -> m Double
valueToNumber v = case v of
  NumberValue n -> return n
  _ -> throwError $ TypeMismatchError "number" (valueGetType v)

valueToString :: MonadError LoxError m => Value -> m StringType
valueToString v = case v of
  StringValue s -> return s
  _ -> throwError $ TypeMismatchError "string" (valueGetType v)

valueCoerceBool :: Value -> Bool
valueCoerceBool v = case v of
  BooleanValue b -> b
  NilValue -> False
  _ -> True

valueToBool :: MonadError LoxError m => Value -> m Bool
valueToBool v = case v of
  BooleanValue b -> return b
  _ -> throwError $ TypeMismatchError "bool" (valueGetType v)

valueToFunction :: MonadError LoxError m => Value -> m LoxFunction
valueToFunction v = case v of
  FunctionValue f -> return f
  _ -> throwError $ TypeMismatchError "function" (valueGetType v)

valueToInstance :: MonadError LoxError m => Value -> m LoxInstance
valueToInstance v = case v of
  InstanceValue i -> return i
  _ -> throwError $ TypeMismatchError "instance" (valueGetType v)

valueAdd :: MonadError LoxError m => Value -> Value -> m Value
valueAdd a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a + b

valueSub :: MonadError LoxError m => Value -> Value -> m Value
valueSub a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a - b

valueMul :: MonadError LoxError m => Value -> Value -> m Value
valueMul a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a * b

valueDiv :: MonadError LoxError m => Value -> Value -> m Value
valueDiv a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ NumberValue $ a / b

valueNegate :: MonadError LoxError f => Value -> f Value
valueNegate a = NumberValue . negate <$> valueToNumber a

valueEqual :: MonadError LoxError m => Value -> Value -> m Bool
valueEqual a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ a == b

valueGreater :: MonadError LoxError m => Value -> Value -> m Bool
valueGreater a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ a > b

valueLess :: MonadError LoxError m => Value -> Value -> m Bool
valueLess a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  return $ a < b

runIOResult :: IOResult String -> IO String
runIOResult action =
  runExceptT (action `catchError` (return . show)) <&> \case
    Right x -> x
    Left x -> show x

liftMaybe :: (MonadError LoxError m) => LoxError -> Maybe a -> m a
liftMaybe _ (Just a) = return a
liftMaybe err Nothing = throwError err
