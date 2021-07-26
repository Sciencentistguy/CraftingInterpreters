{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Loops
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Stack
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import Interpreter.Environment
import Interpreter.Native
import Types
import Util

run :: Vector Instruction -> IOResult ()
run instructions = do
  callStackPtr <- liftIO $ newIORef $ stackPush stackNew $ cfNew (-1)
  programCounterPtr <- liftIO $ newIORef 0
  environmentPtr <- liftIO $ newIORef emptyEnvironment
  addNativeFunction environmentPtr $ NativeFunction "nativeAdd" 2 nativeAdd

  whileM_
    do
      pc <- liftIO $ readIORef programCounterPtr
      return $ pc < length instructions
    do
      pc <- liftIO $ readIORef programCounterPtr
      runInstr callStackPtr programCounterPtr environmentPtr (instructions ! pc)
      liftIO $ modifyIORef' programCounterPtr (+ 1)

  liftIO $ readIORef callStackPtr >>= showCallStack
  do
    s <- liftIO $ readIORef environmentPtr
    printEnvironment s

runInstr :: IORef (Stack CallFrame) -> IORef Int -> IORef Environment -> Instruction -> IOResult ()
runInstr callStackPtr programCounterPtr environmentPtr instr = do
  liftIO $ readIORef callStackPtr >>= showCallStack
  liftIO $readIORef callStackPtr >>= print
  liftIO (readIORef environmentPtr) >>= printEnvironment
  liftIO $putStrLn $ "Executing instruction '" ++ show instr ++ "'"
  case instr of
    PopInstr -> void pop'
    PrintInstr -> do
      a <- pop'
      liftIO $ print a
    ConstantInstr val -> do
      push' val
    AddInstr -> do
      b <- pop'
      a <- pop'
      res <- valueAdd a b
      push' res
    SubInstr -> do
      b <- pop'
      a <- pop'
      res <- valueSub a b
      push' res
    MultiplyInstr -> do
      b <- pop'
      a <- pop'
      res <- valueMul a b
      push' res
    DivideInstr -> do
      b <- pop'
      a <- pop'
      res <- valueDiv a b
      push' res
    NegateInstr -> do
      a <- pop'
      num <- valueToNumber a
      push' $ NumberValue . negate $ num
    NotInstr -> do
      a <- pop'
      push' $ BooleanValue $ not $ valueCoerceBool a
    OrInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ valueCoerceBool a || valueCoerceBool b
    AndInstr -> do
      b <- pop'
      a <- pop'
      push' $ BooleanValue $ valueCoerceBool a && valueCoerceBool b
    EqInstr -> do
      b <- pop'
      a <- pop'
      res <- valueEqual a b
      push' $ BooleanValue res
    GreaterInstr -> do
      b <- pop'
      a <- pop'
      res <- valueGreater a b
      push' $ BooleanValue res
    GreaterEqInstr -> do
      b <- pop'
      a <- pop'
      res <- liftA2 (||) (valueGreater a b) (valueEqual a b)
      push' $ BooleanValue res
    LessInstr -> do
      b <- pop'
      a <- pop'
      res <- valueLess a b
      push' $ BooleanValue res
    LessEqInstr -> do
      b <- pop'
      a <- pop'
      res <- liftA2 (||) (valueLess a b) (valueEqual a b)
      push' $ BooleanValue res
    DefineVariableInstr name -> do
      exists <- liftIO $ checkVariableExistsInCurrentScope name
      when exists $ throwError $ VariableExistsError $ T.unpack name
      a <- pop'
      liftIO $ addVariable name a
    GetVariableInstr name -> do
      val <- getVariable name
      push' val
    SetVariableInstr name -> do
      val <- peek'
      updateVariable name val
    BeginScopeInstr -> do
      liftIO $ modifyIORef' environmentPtr newScope
    EndScopeInstr -> do
      modifyIORefM environmentPtr popScope_
    JumpIfFalseInstr distance -> do
      unlessM (valueCoerceBool <$> peek') $ liftIO $ modifyIORef' programCounterPtr (+ distance)
      void pop'
    JumpInstr distance -> do
      liftIO $ modifyIORef' programCounterPtr (+ distance)
    DefineFunctionInstr -> do
      pc <- liftIO $ readIORef programCounterPtr
      LoxFunction {..} <- do
        x <- pop'
        valueToFunction x
      let function' = LoxFunction (Just $ pc + 2) lfArity lfName
      push' $ FunctionValue function'
    CallInstr -> do
      stack <- getLocalStack
      callable <- do
        let isCallable v = case v of
              FunctionValue _ -> True
              ClassValue _ -> True
              _ -> False
        liftMaybe (GenericError "Attempted to call with no callable in the stack") $
          findFirst stack isCallable
      case callable of
        FunctionValue LoxFunction {..} -> do
          location <- liftMaybe (InternalError "Incorrectly initialised function") lfLocation
          pc <- liftIO $ readIORef programCounterPtr
          stack <- getLocalStack
          let newFrame = CallFrame stack pc
          liftIO $ modifyIORef' callStackPtr $ \x -> stackPush x newFrame
          liftIO $ writeIORef programCounterPtr location
        FunctionValue NativeFunction {..} -> do
          args <- reverse <$> replicateM nfArity pop'
          value <- nfFunction args
          push' value
        ClassValue class' -> do
          env <- liftIO $ newIORef HashMap.empty
          let inst = LoxInstance class' env
          push' $ InstanceValue inst
        other -> throwError $ TypeMismatchError "callable" (valueGetType other)
    ReturnInstr -> do
      callStack <- liftIO $ readIORef callStackPtr
      returnValue <- pop'
      (callStack', CallFrame _ location) <-
        liftMaybe
          (InternalError "Empty call stack")
          $ stackPop callStack
      liftIO $ writeIORef programCounterPtr location
      liftIO $ writeIORef callStackPtr callStack'
      push' returnValue
      modifyIORefM environmentPtr popScope_ -- leave function scope after returning
    DefineClassInstr name -> do
      let class' = ClassValue (LoxClass name)
      push' class'
    GetPropInstr propName -> do
      LoxInstance (LoxClass name) envPtr <- valueToInstance =<< pop'
      instanceEnv <- liftIO $ readIORef envPtr
      let val = HashMap.lookup propName instanceEnv
      case val of
        Nothing -> throwError $ UnboundVariableError (T.unpack name ++ "." ++ T.unpack propName)
        Just val -> do
          val <- liftIO $ readIORef val
          push' val
    SetPropInstr propName -> do
      (LoxInstance _ envPtr) <- valueToInstance =<< pop'
      val <- pop'
      instanceEnv <- liftIO $ readIORef envPtr
      valPtr <- liftIO $ newIORef val
      let instanceEnv' = HashMap.insert propName valPtr instanceEnv
      liftIO $ writeIORef envPtr instanceEnv'
      push' val
  where
    pop' :: (MonadIO m, MonadError LoxError m) => m Value
    pop' = do
      callStack <- liftIO $ readIORef callStackPtr
      (callStack', frame@(CallFrame stack _)) <-
        liftMaybe (InternalError "Empty call stack") $
          stackPop callStack
      (stack', value) <-
        liftMaybe
          (InternalError "Attempted to pop from empty stack")
          $ stackPop stack
      let frame' = cfReplaceStack frame stack'
      let callStack'' = stackPush callStack' frame'
      liftIO $ writeIORef callStackPtr callStack''
      return value

    push' :: (MonadIO m, MonadError LoxError m) => Value -> m ()
    push' value = do
      callStack <- liftIO $ readIORef callStackPtr
      (callStack', frame@(CallFrame stack _)) <-
        liftMaybe
          (InternalError "Empty call stack")
          $ stackPop callStack
      let stack' = stackPush stack value
      let frame' = cfReplaceStack frame stack'
      let callStack'' = stackPush callStack' frame'
      liftIO $ writeIORef callStackPtr callStack''

    peek' :: (MonadIO m, MonadError LoxError m) => m Value
    peek' = do
      callstack <- liftIO $ readIORef callStackPtr
      CallFrame stack _ <- liftMaybe (InternalError "Empty call stack") $ stackPeek callstack
      liftMaybe (InternalError "Attempted to peek empty stack") $ stackPeek stack

    checkVariableExistsInCurrentScope key = liftIO do
      env <- readIORef environmentPtr
      return $ HashMap.member key $ liftEnv head env

    getVariablePtr key = do
      env <- liftIO $ readIORef environmentPtr
      let f = map $ HashMap.lookup key
          x = liftEnv f env
          y = asum x
      case y of
        Just x -> return x
        Nothing -> throwError $ UnboundVariableError (T.unpack key)

    getVariable key = do
      ptr <- getVariablePtr key
      liftIO $ readIORef ptr

    updateVariable key value = do
      variablePtr <- getVariablePtr key
      liftIO $ writeIORef variablePtr value

    addVariable key value = liftIO $ do
      var <- newIORef value
      modifyIORef'
        environmentPtr
        \(Environment (x : xs)) -> Environment $ HashMap.insert key var x : xs

    getLocalStack :: (MonadIO m, MonadError LoxError m) => m (Stack Value)
    getLocalStack = do
      callstack <- liftIO $ readIORef callStackPtr
      let top = stackPeek callstack
          stack = cfStack <$> top
      case stack of
        Just s -> return s
        Nothing -> throwError $ InternalError "Empty call stack"
