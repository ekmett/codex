{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
{-# language LambdaCase #-}
{-# language TypeFamilies #-}
module Engine.Include.Cache
( deps
, IncludeCache
, GivenIncludeCache
) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Watch
import Data.Watch.Directory

import Engine.Include.Body

type IncludeCache = HashMap FilePath (IOThunk Body)

type GivenIncludeCache = (?includes :: MVar IncludeCache, GivenDirectoryWatcher)

-- returns a thunk that has data dependencies on all includes mentioned
deps :: (GivenIncludeCache, MonadIO m) => Body -> m (IOThunk IncludeCache)
deps body = liftIO $ delay $ execStateT (forOf_ paths body cache) HashMap.empty

-- | Path should be absolute
includes :: (GivenIncludeCache, MonadIO m) => FilePath -> m (IOThunk Body)
includes path = liftIO $ do
  modifyMVar ?includes $ \m -> case m^.at path of
    Just body -> pure (m,body)
    Nothing -> do
      body <- delay $ readWatchedFile path >>= (force >=> absolve)
      pure (HashMap.insert path body m,body)

cache :: (GivenIncludeCache, MonadWatch m, PrimState m ~ RealWorld) => FilePath -> StateT IncludeCache m ()
cache path = use (at path) >>= \case
  Just _ -> pure ()
  Nothing -> do
    bt <- ioToPrim $ includes path
    at path ?= bt
    body <- force bt
    forOf_ paths body cache -- walk the body
{-# inline cache #-}
