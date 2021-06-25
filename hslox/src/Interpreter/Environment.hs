{-# LANGUAGE BlockArguments #-}

module Interpreter.Environment where

import Control.Monad
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Instructions

newtype Environment = Environment [HashMap StringType Value]

liftEnv f (Environment ls) = f ls

mapEnv f (Environment ls) = Environment $ f ls

newScope (Environment ls) = Environment $ HashMap.empty : ls

popScope_ :: Environment -> Environment
popScope_ env@(Environment ls) = case ls of
  [] -> error "Internal: Empty Environment should be impossible"
  [x] -> error "Internal: Attempted to pop global scope"
  _ -> mapEnv tail env

emptyEnvironment = Environment [HashMap.empty]

globals (Environment x) = case x of
  [] -> error "Internal: Empty Environment should be impossible"
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
