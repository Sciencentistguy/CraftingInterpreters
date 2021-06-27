{-# LANGUAGE RecordWildCards #-}

module Interpreter.Function (LoxFunction) where

import Data.Text (Text)
import qualified Data.Text as T
import Interpreter.Environment

type StringType = Text

data LoxFunction arg_type
  = LoxFunction
      { lfLocation :: Maybe Int,
        lfArity :: Int,
        lfName :: StringType,
        lfEnvironment :: Environment
      }
  | NativeFunction
      { nfName :: StringType,
        nfArity :: Int,
        nfFunction :: [arg_type] -> IO arg_type
      }

instance Show a => Show (LoxFunction a) where
  show LoxFunction {..} = "<fn " ++ T.unpack lfName ++ "(" ++ show lfArity ++ ")>"
  show NativeFunction {..} = "<native fn " ++ T.unpack nfName ++ "(" ++ show nfArity ++ ")>"
