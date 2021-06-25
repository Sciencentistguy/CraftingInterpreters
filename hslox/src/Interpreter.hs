{-# LANGUAGE BlockArguments #-}

module Interpreter where

import AST
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.IORef
import Data.Stack
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace
import Instructions
import Interpreter.Environment

run :: Vector Instruction -> IO ()
run instructions = do
  stackPtr <- newIORef stackNew
  programCounterPtr <- newIORef 0
  environmentPtr <- newIORef emptyEnvironment
  whileM_
    do
      pc <- readIORef programCounterPtr
      return $ pc < length instructions
    do
      pc <- readIORef programCounterPtr
      runInstr stackPtr programCounterPtr environmentPtr (instructions ! pc)
      modifyIORef' programCounterPtr (+ 1)
  readIORef stackPtr >>= print
  do
    s <- readIORef environmentPtr
    printEnvironment s
    return ()

runInstr :: IORef (Stack Value) -> IORef Int -> IORef Environment -> Instruction -> IO ()
runInstr stackPtr programCounterPtr environmentPtr instr = do
  readIORef stackPtr >>= print
  do
    s <- readIORef environmentPtr
    printEnvironment s
    return ()
  putStrLn $ "Executing instruction '" ++ show instr ++ "'"
  case instr of
    ReturnInstr -> do
      void pop'
    PrintInstr -> do
      a <- pop'
      print a
    ConstantInstr val -> do
      push stackPtr val
    AddInstr -> do
      b <- pop'
      a <- pop'
      push' $ handleError $ valueAdd a b
    SubInstr -> do
      b <- pop'
      a <- pop'
      push' $ handleError $ valueSub a b
    MultiplyInstr -> do
      b <- pop'
      a <- pop'
      push' $ handleError $ valueMul a b
    DivideInstr -> do
      b <- pop'
      a <- pop'
      push' $ handleError $ valueDiv a b
    NegateInstr -> do
      a <- pop'
      push' $
        handleError $
          errorMsg "Operand to unary '-' must be a number" $
            NumberValue . negate <$> valueToNumber a
    NotInstr -> do
      a <- pop'
      push' $ BooleanValue $ not $valueToBool a
    OrInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ valueToBool a || valueToBool b
    AndInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ valueToBool a && valueToBool b
    EqInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ handleError $ valueEqual a b
    GreaterInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ handleError $ valueGreater a b
    GreaterEqInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ handleError $ liftA2 (||) (valueGreater a b) (valueEqual a b)
    LessInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ handleError $ valueLess a b
    LessEqInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ handleError $ liftA2 (||) (valueLess a b) (valueEqual a b)
    DefineVariableInstr name -> do
      exists <- checkVaraibleExistsInCurrentScope name
      if exists
        then error $ "Variable with name '" ++ T.unpack name ++ "' already exists in the current scope."
        else do
          a <- pop'
          addVariable name a
    GetVariableInstr name -> do
      val <- getVariable name
      case val of
        Just v -> push' v
        Nothing -> error $ "Variable with name '" ++ T.unpack name ++ "' does not exist."
    SetVariableInstr name -> do
      contains <- checkVaraibleExists name
      val <- peek'
      if not contains
        then error $ "Variable with name '" ++ T.unpack name ++ "' does not exist."
        else updateVariable name val
    BeginScopeInstr -> do
      modifyIORef' environmentPtr newScope
    EndScopeInstr -> do
      modifyIORef' environmentPtr popScope_
  where
    pop' = pop stackPtr
    push' = push stackPtr
    peek' = peek stackPtr

    checkVaraibleExists key = do
      env <- readIORef environmentPtr
      let f = map $ HashMap.member key
      return $ or $ liftEnv f env

    checkVaraibleExistsInCurrentScope key = do
      env <- readIORef environmentPtr
      return $ HashMap.member key $ liftEnv head env

    getVariable key = do
      env <- readIORef environmentPtr
      let f = map $ HashMap.lookup key
      return (asum $ liftEnv f env)

    updateVariable key value = do
      Environment ls <- readIORef environmentPtr
      let (updatedMap, index) = go ls 0
      let newEnv = Environment $ replaceInList ls index updatedMap
      writeIORef environmentPtr newEnv
      where
        replaceInList xs n newElement = take n xs ++ [newElement] ++ drop (n + 1) xs
        go [] _ = error "variable does not exist"
        go (x : xs) idx =
          if HashMap.member key x
            then (HashMap.insert key value x, idx)
            else go xs (idx + 1)

    addVariable key value = modifyIORef'
      environmentPtr
      \(Environment (x : xs)) -> Environment $ HashMap.insert key value x : xs

push :: IORef (Stack a) -> a -> IO ()
push stackPtr val = modifyIORef' stackPtr $ \stack -> stackPush stack val

pop :: IORef (Stack a) -> IO a
pop stackPtr = do
  stack <- readIORef stackPtr
  let (newStack, val) = case stackPop stack of
        Nothing -> error "Attempted to pop from empty stack."
        Just a -> a
  writeIORef stackPtr newStack
  return val

peek :: IORef (Stack a) -> IO a
peek stackPtr = do
  stack <- readIORef stackPtr
  return $ case stackPeek stack of
    Nothing -> error "Attempted to peek empty stack."
    Just a -> a

handleError :: Either String a -> a
-- TODO I should probably be using ExceptT but I don't understand it so `error` it is
handleError either = case either of
  Left err -> error err
  Right a -> a
