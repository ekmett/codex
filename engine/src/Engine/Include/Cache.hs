{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
module Engine.Include.Cache
( body
, deps
, Body(..)
, IncludeCache
, GivenIncludeCache
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Watch
import Data.Watch.Directory
import System.Directory

import Engine.Include.Body

type IncludeCache = HashMap FilePath (IOThunk Body)

type GivenIncludeCache = (?includes :: MVar IncludeCache, GivenDirectoryWatcher)

-- returns a thunk that has data dependencies on all includes mentioned
deps :: (GivenIncludeCache, MonadIO m) => Body -> m (IOThunk IncludeCache)
deps body = delay $ execStateT (forOf_ each body cache) HashMap.empty

-- | Path should be absolute
includes :: (GivenIncludeCache, MonadIO m) => FilePath -> m (IOThunk Body)
includes path = liftIO $ do
  modifyMVar cache $ \m -> case m^.at path of
    Just body -> pure (m,body)
    Nothing -> do
      body <- delay $ readWatchedFile file >>= absolve
      pure (HashMap.insert path body,body)

cache :: (GivenIncludeCache, MonadWatch m) => FilePath -> StateT Cache m ()
cache path = use (at path) >>= \case
  Just a -> pure ()
  Nothing -> do
    bt <- includes path
    at path ?= bt
    body <- force bt
    forOf_ each body cache -- walk the body
