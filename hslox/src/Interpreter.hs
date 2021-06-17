{-# LANGUAGE BlockArguments #-}

module Interpreter where

import AST
import Control.Applicative
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.IORef
import Data.Stack
import qualified Data.Text as T
import Debug.Trace
import Instructions

run :: [Instruction] -> IO ()
run instructions = do
  stackPtr <- newIORef stackNew
  variablesMapPtr <- newIORef HashMap.empty
  readIORef stackPtr >>= print
  putStrLn "Variables: "
  readIORef variablesMapPtr >>= print
  mapM_ (runInstr stackPtr variablesMapPtr) instructions

runInstr :: IORef (Stack Value) -> IORef (HashMap StringType Value) -> Instruction -> IO ()
runInstr stackPtr variablesMapPtr instr = do
  putStrLn $ "Executing instruction '" ++ show instr ++ "'"
  readIORef stackPtr >>= print
  putStrLn "Variables: "
  printVariables variablesMapPtr
  case instr of
    ReturnInstr -> do
      void pop'
    --readIORef stackPtr >>= print
    --putStrLn "Variables: "
    --readIORef variablesMapPtr >>= print
    PrintInstr -> do
      a <- pop'
      print a
    ConstantInstr val -> do
      push stackPtr val
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
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ valueToBool a || valueToBool b
    AndInstr -> do
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ valueToBool a && valueToBool b
    EqInstr -> do
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ handleError $ valueEqual a b
    GreaterInstr -> do
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ handleError $ valueGreater a b
    GreaterEqInstr -> do
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ handleError $ liftA2 (||) (valueGreater a b) (valueEqual a b)
    LessInstr -> do
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ handleError $ valueLess a b
    LessEqInstr -> do
      a <- pop'
      b <- pop'
      push' $ BooleanValue $ handleError $ liftA2 (||) (valueLess a b) (valueEqual a b)
    DefineGlobalInstr name -> do
      contains <- contains' name
      if contains
        then error $ "Variable with name '" ++ T.unpack name ++ "' already exists."
        else do
          a <- pop'
          insert' name a
    GetGlobalInstr name -> do
      val <- retrieve' name
      case val of
        Just v -> push' v
        Nothing -> error $ "Variable with name '" ++ T.unpack name ++ "' does not exist."
    SetGlobalInstr name -> do
      contains <- contains' name
      val <- peek'
      if not contains
        then error $ "Variable with name '" ++ T.unpack name ++ "' does not exist."
        else insert' name val
  where
    pop' = pop stackPtr
    push' = push stackPtr
    peek' = peek stackPtr
    contains' = contains variablesMapPtr
    insert' = insert variablesMapPtr
    retrieve' = retrieve variablesMapPtr

contains :: (Eq k, Hashable k) => IORef (HashMap k v) -> k -> IO Bool
contains mapPtr key = do
  map <- readIORef mapPtr
  return $ HashMap.member key map

insert :: (Eq k, Hashable k) => IORef (HashMap k v) -> k -> v -> IO ()
insert mapPtr key val = modifyIORef' mapPtr $ HashMap.insert key val

retrieve :: (Eq k, Hashable k) => IORef (HashMap k v) -> k -> IO (Maybe v)
retrieve mapPtr key = do
  map <- readIORef mapPtr
  return $ HashMap.lookup key map

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

printVariables :: IORef (HashMap StringType Value) -> IO ()
printVariables map = do
  map <- readIORef map
  let pairs = HashMap.toList map
  mapM_ printBinding pairs
  where
    printBinding (k, v) = putStrLn $ "\t'" ++ T.unpack k ++ "' = " ++ show v ++ "."
