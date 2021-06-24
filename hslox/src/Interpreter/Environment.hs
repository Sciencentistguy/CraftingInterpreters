module Interpreter.Environment where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Instructions

newtype Environment = Environment [HashMap StringType Value]

liftEnv f (Environment ls) = f ls

mapEnv f (Environment ls) = Environment $ f ls

newScope (Environment ls) = Environment $ HM.empty : ls

popScope_ :: Environment -> Environment
popScope_ env@(Environment ls) = case ls of
  [] -> error "Internal: empty Environment should be impossible"
  [x] -> error "Internal: Attempted to pop global scope"
  _ -> mapEnv tail env

emptyEnvironment = Environment [HM.empty]

globals (Environment x) = case x of
  [] -> error "empty Environment should be impossible"
  _ -> last x

--globals (Scope vars enclosing) = case enclosing of
--Just a -> globals a
--Nothing -> vars

--getAtDepth :: Environment -> Int -> Environment
getAtDepth = liftEnv (!!)

--getDepth scope depth = case depth of
--0 -> Just scope
--_ -> case getParentScope scope of
--Just a -> getDepth a (depth - 1)
--Nothing -> Nothing
