module Interpreter.Error where

internalError :: String -> a
internalError msg = error $ "Internal: " ++ msg

handleError :: Either String a -> a
-- TODO I should probably be using ExceptT but I don't understand it so `error` it is
handleError either = case either of
  Left err -> error err
  Right a -> a

errorMsg :: a -> Maybe b -> Either a b
errorMsg _ (Just a) = Right a
errorMsg msg Nothing = Left msg
