{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter where

import AST
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Loops
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.IORef
import Data.Maybe
import Data.Stack
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace
import Instruction
import Interpreter.Environment
import Interpreter.Error
import Interpreter.Native
import Value

run :: Vector Instruction -> IO ()
run instructions = do
  callStackPtr <- newIORef $ stackPush stackNew $ cfNew (-1)
  programCounterPtr <- newIORef 0
  environmentPtr <- newIORef emptyEnvironment
  addNativeFunction environmentPtr $ NativeFunction "nativeAdd" 2 nativeAdd

  whileM_
    do
      pc <- readIORef programCounterPtr
      return $ pc < length instructions
    do
      pc <- readIORef programCounterPtr
      runInstr callStackPtr programCounterPtr environmentPtr (instructions ! pc)
      modifyIORef' programCounterPtr (+ 1)

  readIORef callStackPtr >>= showCallStack
  do
    s <- readIORef environmentPtr
    printEnvironment s

runInstr :: IORef (Stack CallFrame) -> IORef Int -> IORef Environment -> Instruction -> IO ()
runInstr callStackPtr programCounterPtr environmentPtr instr = do
  readIORef callStackPtr >>= showCallStack
  readIORef callStackPtr >>= print
  readIORef environmentPtr >>= printEnvironment
  putStrLn $ "Executing instruction '" ++ show instr ++ "'"
  case instr of
    PopInstr -> do
      void pop'
    PrintInstr -> do
      a <- pop'
      print a
    ConstantInstr val -> do
      push' val
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
      push' $ BooleanValue $ not $ valueToBool a
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
      exists <- checkVariableExistsInCurrentScope name
      when exists $
        error $
          "Variable with name '" ++ T.unpack name ++ "' already exists in the current scope."
      a <- pop'
      addVariable name a
    GetVariableInstr name -> do
      val <- getVariable name
      case val of
        Just v -> push' v
        Nothing -> error $ "Variable with name '" ++ T.unpack name ++ "' does not exist."
    SetVariableInstr name -> do
      contains <- checkVariableExists name
      unless contains $ error $ "Variable with name '" ++ T.unpack name ++ "' does not exist."
      val <- peek'
      updateVariable name val
    BeginScopeInstr -> do
      modifyIORef' environmentPtr newScope
    EndScopeInstr -> do
      modifyIORef' environmentPtr popScope_
    JumpIfFalseInstr distance -> do
      unlessM (valueToBool <$> peek') $ modifyIORef' programCounterPtr (+ distance)
      void pop'
    JumpInstr distance -> do
      modifyIORef' programCounterPtr (+ distance)
    DefineFunctionInstr -> do
      pc <- readIORef programCounterPtr
      LoxFunction {..} <-
        fromMaybe
          (internalError "Cannot call DefineFunctionInstr on not a function")
          . valueToFunction
          <$> pop'
      let function' = LoxFunction (Just $ pc + 2) lfArity lfName
      push' $ FunctionValue function'
    CallInstr -> do
      stack <- getLocalStack
      let function = fromMaybe (internalError "Attempted to call when there is no function in the stack") do
            let isFunction = isJust . valueToFunction
            v <- findFirst stack isFunction
            valueToFunction v
      case function of
        LoxFunction {..} -> do
          let location = case lfLocation of
                Nothing -> internalError "Incorrectly initialised function"
                Just a -> a
          pc <- readIORef programCounterPtr
          stack <- getLocalStack
          let newFrame = CallFrame stack pc
          modifyIORef' callStackPtr $ \x -> stackPush x newFrame
          writeIORef programCounterPtr location
        NativeFunction {..} -> do
          args <- reverse <$> replicateM nfArity pop'
          value <- nfFunction args
          push' value
    ReturnInstr -> do
      callStack <- readIORef callStackPtr
      returnValue <- pop'
      let (callStack', CallFrame _ location) =
            fromMaybe
              (internalError "Empty call stack")
              $ stackPop callStack
      writeIORef programCounterPtr location
      writeIORef callStackPtr callStack'
      push' returnValue
      modifyIORef' environmentPtr popScope_ -- leave function scope after returning
  where
    pop' :: IO Value
    pop' = do
      callStack <- readIORef callStackPtr
      let (callStack', frame@(CallFrame stack _)) =
            fromMaybe
              (internalError "Empty call stack")
              $ stackPop callStack
      let (stack', value) = case stackPop stack of
            Just a -> a
            Nothing -> internalError "Attempted to pop from empty stack"
      let frame' = cfReplaceStack frame stack'
      let callStack'' = stackPush callStack' frame'
      writeIORef callStackPtr callStack''
      return value

    push' :: Value -> IO ()
    push' value = do
      callStack <- readIORef callStackPtr
      let (callStack', frame@(CallFrame stack _)) =
            fromMaybe
              (internalError "Empty call stack")
              $ stackPop callStack
      let stack' = stackPush stack value
      let frame' = cfReplaceStack frame stack'
      let callStack'' = stackPush callStack' frame'
      writeIORef callStackPtr callStack''

    peek' :: IO Value
    peek' = do
      CallFrame stack _ <-
        fromMaybe (internalError "Empty call stack")
          . stackPeek
          <$> readIORef callStackPtr
      return $ case stackPeek stack of
        Nothing -> internalError "Attempted to peek empty stack"
        Just a -> a

    checkVariableExists key = do
      env <- readIORef environmentPtr
      let f = map $ HashMap.member key
      return $ or $ liftEnv f env

    checkVariableExistsInCurrentScope key = do
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
        go [] _ = internalError "Variable does not exist"
        go (x : xs) idx =
          if HashMap.member key x
            then (HashMap.insert key value x, idx)
            else go xs (idx + 1)

    addVariable key value = modifyIORef'
      environmentPtr
      \(Environment (x : xs)) -> Environment $ HashMap.insert key value x : xs

    getLocalStack :: IO (Stack Value)
    getLocalStack =
      -- readIORef callStackPtr returns an IO (Stack CallFrame). mapping stackPeek :: Stack a ->
      -- Maybe a gives an IO (Maybe (CallFrame)). double-mapping cfStack :: CallFrame ->
      -- Stack Value then gives an IO (Maybe (Stack Value)). Mapping fromMaybe over this gives the
      -- required IO (Stack Value)
      fmap
        (fromMaybe (internalError "Empty call stack"))
        (fmap cfStack . stackPeek <$> readIORef callStackPtr)
