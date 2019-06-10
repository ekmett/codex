{-# language DeriveAnyClass #-}
module Engine.Exception
( Shutdown(..)
, AsShutdown(..)
, shutdown
) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens.Type
import Control.Monad.IO.Class

data Shutdown = Shutdown deriving (Show,Exception)
class AsShutdown t where _Shutdown :: Prism' t Shutdown
instance AsShutdown Shutdown where _Shutdown = id
instance AsShutdown SomeException where _Shutdown = exception

shutdown :: MonadIO m => m a
shutdown = liftIO $ throwIO Shutdown
