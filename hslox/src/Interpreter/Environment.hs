{-# LANGUAGE BlockArguments #-}

module Interpreter.Environment where

import Control.Monad
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List
import Data.Maybe
import Data.Stack
import qualified Data.Text as T
import Interpreter.Error
import Value

newtype Environment = Environment [HashMap StringType Value]

liftEnv f (Environment ls) = f ls

mapEnv f (Environment ls) = Environment $ f ls

newScope (Environment ls) = Environment $ HashMap.empty : ls

-- | Pop a scope and discard it, returning the modified environment
popScope_ :: Environment -> Environment
popScope_ env@(Environment ls) = case ls of
  [] -> internalError "Empty Environment should be impossible"
  [x] -> internalError "Attempted to pop global scope"
  _ -> mapEnv tail env

emptyEnvironment = Environment [HashMap.empty]

globals (Environment x) = case x of
  [] -> internalError "Empty Environment should be impossible"
  _ -> last x

printEnvironment :: Environment -> IO ()
printEnvironment env = do
  let gs = globals env
  unless (null gs) do
    putStrLn "Global variables:"
    let pairs = HashMap.toList gs
    mapM_ printBinding pairs
  let scopes = reverse <$> liftEnv initMaybe env
  when (isJust scopes) $ putStrLn "Scoped variables:"
  forM_ scopes (mapM_ f)
  where
    printBinding (k, v) = putStrLn $ "  '" ++ T.unpack k ++ "' = " ++ show v ++ "."
    initMaybe [] = Nothing
    initMaybe x = Just $ init x

    f scope = unless (null scope) do
      let pairs = HashMap.toList scope
      mapM_ printBinding pairs

push :: IORef (Stack a) -> a -> IO ()
push stackPtr val = modifyIORef' stackPtr $ \stack -> stackPush stack val

pop :: IORef (Stack a) -> IO a
pop stackPtr = do
  stack <- readIORef stackPtr
  let (newStack, val) = case stackPop stack of
        Nothing -> internalError "Attempted to pop from empty stack."
        Just a -> a
  writeIORef stackPtr newStack
  return val

peek :: IORef (Stack a) -> IO a
peek stackPtr = do
  stack <- readIORef stackPtr
  return $ case stackPeek stack of
    Nothing -> internalError "Attempted to peek empty stack."
    Just a -> a

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

cfStackMap f (CallFrame stack a) = CallFrame (f stack) a

cfReplaceStack (CallFrame _ a) stack = CallFrame stack a

showCallStack callStack =
  let CallFrame stack _ = fromJust $ stackPeek callStack
   in print stack

cfNew = CallFrame stackNew
