module Util where

import Control.Monad.IO.Class
import Data.IORef

modifyIORefM :: (MonadIO m) => IORef a -> (a -> m a) -> m ()
modifyIORefM ptr f = do
  val <- liftIO $ readIORef ptr
  val' <- f val
  liftIO $ writeIORef ptr val'
