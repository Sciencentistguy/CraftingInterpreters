{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Environment where

import Control.Monad
import Control.Monad.Except
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe
import Data.Stack
import qualified Data.Text as T
import Safe
import Types

newtype Environment = Environment [HashMap StringType (IORef Value)]

liftEnv :: ([HashMap StringType (IORef Value)] -> t) -> Environment -> t
liftEnv f (Environment ls) = f ls

mapEnv ::
  ([HashMap StringType (IORef Value)] -> [HashMap StringType (IORef Value)]) ->
  Environment ->
  Environment
mapEnv f (Environment ls) = Environment $ f ls

newScope :: Environment -> Environment
newScope (Environment ls) = Environment $ HashMap.empty : ls

-- | Pop a scope and discard it, returning the modified environment
popScope_ :: (MonadError LoxError m) => Environment -> m Environment
popScope_ env@(Environment ls) = case ls of
  [] -> throwError $ InternalError "Environment is empty"
  [_] -> throwError $ InternalError "Attempted to pop global scope"
  _ -> return $ mapEnv tail env

emptyEnvironment :: Environment
emptyEnvironment = Environment [HashMap.empty]

globals :: MonadError LoxError m => Environment -> m (HashMap StringType (IORef Value))
globals (Environment x) = liftMaybe (InternalError "Envrionment is empty") $ lastMay x

printEnvironment :: (MonadError LoxError m, MonadIO m) => Environment -> m ()
printEnvironment env = do
  gs <- globals env
  unless (null gs) $ liftIO do
    putStrLn "Global variables:"
    let pairs = HashMap.toList gs
    traverse_ printBinding pairs
  let scopes = reverse <$> liftEnv initMay env
  liftIO $ when (isJust scopes) $ putStrLn "Scoped variables:"
  liftIO $ forM_ scopes (mapM_ f)
  where
    printBinding (k, v) = do
      v <- readIORef v
      putStrLn $ "  '" ++ T.unpack k ++ "' = " ++ show v ++ "."

    f scope = unless (null scope) do
      let pairs = HashMap.toList scope
      mapM_ printBinding pairs

findFirst :: Stack a -> (a -> Bool) -> Maybe a
findFirst stack p = do
  (stack', val) <- stackPop stack
  if p val
    then return val
    else findFirst stack' p

data CallFrame = CallFrame
  { cfStack :: Stack Value,
    cfReturnAddr :: Int
  }
  deriving (Show)

cfStackMap :: (Stack Value -> Stack Value) -> CallFrame -> CallFrame
cfStackMap f (CallFrame stack a) = CallFrame (f stack) a

cfReplaceStack :: CallFrame -> Stack Value -> CallFrame
cfReplaceStack (CallFrame _ a) stack = CallFrame stack a

showCallStack :: Stack CallFrame -> IO ()
showCallStack callStack = case stackPeek callStack of
  Just (CallFrame stack _) -> print stack
  Nothing -> return ()

cfNew :: Int -> CallFrame
cfNew = CallFrame stackNew
