{-# LANGUAGE BlockArguments #-}

module Interpreter.Native where

import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe
import Interpreter.Environment
import Interpreter.Error
import Util
import Value

nativeAdd [a, b] = return $ handleError $ valueAdd a b
nativeAdd _ = error "Incorrect number of arguments passed to nativeAdd"

addNativeFunction :: IORef Environment -> LoxFunction -> IO ()
addNativeFunction environmentPtr nativeFunction = do
  env <- readIORef environmentPtr
  let globals' = globals env
  let globals'' =
        HashMap.insert
          (nfName nativeFunction)
          (FunctionValue nativeFunction)
          globals'
  let env' = fromMaybe (internalError "empty environment") do
        x <- liftEnv initMaybe env
        return $ Environment $ x ++ [globals'']
  writeIORef environmentPtr env'

initMaybe [] = Nothing
initMaybe x = Just $ init x
