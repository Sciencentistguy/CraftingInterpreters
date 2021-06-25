{-# LANGUAGE BlockArguments #-}
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
import Instructions
import Interpreter.Environment

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

run :: Vector Instruction -> IO ()
run instructions = do
  callStackPtr <- newIORef $ stackPush stackNew $ cfNew (-1)
  programCounterPtr <- newIORef 0
  environmentPtr <- newIORef emptyEnvironment
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
      let LoxFunction {..} = fromMaybe (internalError "Attempted to call when there is no function in the stack") do
            let g = isJust . valueToFunction
            v <- findFirst stack g
            valueToFunction v
      --fromMaybe (internalError "Attempted to call not a function") . valueToFunction <$> findFirst stack (isJust . valueToFunction)
      let location = case lfLocation of
            Nothing -> internalError "Incorrecntly initialised function"
            Just a -> a
      pc <- readIORef programCounterPtr
      stack <- getLocalStack
      let newFrame = CallFrame stack pc --cfNew pc
      modifyIORef' callStackPtr $ \x -> stackPush x newFrame
      writeIORef programCounterPtr location
    ReturnInstr -> do
      callStack <- readIORef callStackPtr
      returnValue <- pop'
      let (callStack', CallFrame _ location) = fromJust $ stackPop callStack
      writeIORef programCounterPtr location
      writeIORef callStackPtr callStack'
      push' returnValue
      modifyIORef' environmentPtr popScope_ -- leave function scope after returning
  where
    pop' :: IO Value
    pop' = do
      callStack <- readIORef callStackPtr
      let (callStack', frame@(CallFrame stack _)) = fromJust $ stackPop callStack
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
      let (callStack', frame@(CallFrame stack _)) = fromJust $ stackPop callStack
      let stack' = stackPush stack value
      let frame' = cfReplaceStack frame stack'
      let callStack'' = stackPush callStack' frame'
      writeIORef callStackPtr callStack''

    peek' :: IO Value
    peek' = do
      CallFrame stack _ <- fromJust . stackPeek <$> readIORef callStackPtr
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

    getLocalStack = cfStack . fromJust . stackPeek <$> readIORef callStackPtr

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

handleError :: Either String a -> a
-- TODO I should probably be using ExceptT but I don't understand it so `error` it is
handleError either = case either of
  Left err -> error err
  Right a -> a

internalError :: String -> a
internalError msg = error $ "Internal: " ++ msg

findFirst :: Stack a -> (a -> Bool) -> Maybe a
findFirst stack p = do
  (stack', val) <- stackPop stack
  if p val
    then return val
    else findFirst stack' p
