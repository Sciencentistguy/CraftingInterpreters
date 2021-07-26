module Interpreter.Native where

import Control.Monad.Except
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Interpreter.Environment
import Safe
import Types

nativeAdd :: [Value] -> IOResult Value
nativeAdd [a, b] = valueAdd a b
nativeAdd bal = throwError $ NumArgsError 2 bal

addNativeFunction :: (MonadError LoxError m, MonadIO m) => IORef Environment -> LoxFunction -> m ()
addNativeFunction environmentPtr nativeFunction = do
  env <- liftIO $ readIORef environmentPtr
  globals' <- globals env
  var <- liftIO $ newIORef $ FunctionValue nativeFunction
  let globals'' = HashMap.insert (nfName nativeFunction) var globals'
  env' <- liftMaybe (InternalError "empty environment") do
    x <- liftEnv initMay env
    return $ Environment $ x ++ [globals'']
  liftIO $ writeIORef environmentPtr env'
