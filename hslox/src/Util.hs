module Util where

ifM cond then' else' = do
  cond <- cond
  if cond
    then then'
    else else'

whenM cond then' = ifM cond then' (pure ())

unlessM cond = ifM cond (pure ())
