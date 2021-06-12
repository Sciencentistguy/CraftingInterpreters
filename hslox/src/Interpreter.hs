{-# LANGUAGE BlockArguments #-}

module Interpreter where

import AST
import Control.Monad
import Data.IORef
import Data.Stack
import qualified Data.Text as T
import Debug.Trace
import Instructions

run :: [Instruction] -> IO ()
run instructions = do
  stackPtr <- newIORef stackNew
  mapM_ (runInstr stackPtr) instructions

runInstr :: IORef (Stack Value) -> Instruction -> IO ()
runInstr stackPtr instr = do
  readIORef stackPtr >>= print
  case instr of
    ReturnInstr -> void pop'
    PrintInstr -> do
      a <- pop'
      print a
    ConstantInstr val -> push stackPtr val
    AddInstr -> do
      a <- pop'
      b <- pop'
      push' $ handleError $ valueAdd a b
    SubInstr -> do
      a <- pop'
      b <- pop'
      push' $ handleError $ valueSub a b
    MultiplyInstr -> do
      a <- pop'
      b <- pop'
      push' $ handleError $ valueMul a b
    DivideInstr -> do
      a <- pop'
      b <- pop'
      push' $ handleError $ valueDiv a b
    _ -> error "Unsupported instruction"
  where
    pop' = pop stackPtr
    push' = push stackPtr

push :: IORef (Stack a) -> a -> IO ()
push stackPtr val = modifyIORef' stackPtr $ \stack -> stackPush stack val

pop :: IORef (Stack a) -> IO a
pop stackPtr = do
  stack <- readIORef stackPtr
  let (newStack, val) = case stackPop stack of
        Nothing -> error "Attempted to pop from empty stack"
        Just a -> a
  writeIORef stackPtr newStack
  return val

handleError :: Either String a -> a
-- TODO I should probably be using ExceptT but I don't understand it so `error` it is
handleError either = case either of
  Left err -> error err
  Right a -> a
