{-# language StrictData #-}
{-# language TypeFamilies #-}
{-# language ImplicitParams #-} -- I was bound to find a usecase sometime
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
{-# language BlockArguments #-}

-- | Usage sketch:
--
-- @
-- watchProgram :: GivenDirectoryWatcher => IO (Thunk Program)
-- watchProgram w = do
--   vthunk <- delayWithIO delete $ readWatchedFileLazy "shaders/foo.vert" >>= compileShader VertexShader
--   fthunk <- delayWithIO delete $ readWatchedFileLazy "shaders/foo.frag" >>= compileShader FragmentShader
--   delayWithIO delete $ do
--     vs <- force vthunk
--     fs <- force fthunk
--     glAttachShader p vs
--     glAttachShader p fs
--     glLinkProgram p
--
-- main = withDirectoryWatcher do
--   listenToTree "shaders"
--   ...
--   foo <- watchProgram
--   force foo -- make sure it compiles now
--   ...
--   let loop = do
--        ...
--        (currentProgram $=) =<< force foo -- recompiles when either shader file changes
--        ...
--   loop
--   ...
--   release foo -- force cleanup
--
-- @
--
-- A real example would include machinery for catching exceptions during compilation
module Data.Watch.Directory
( DirectoryWatcher(..)
, GivenDirectoryWatcher
, withDirectoryWatcher
, withDirectoryWatcherConf
, startDirectoryWatcher
, startDirectoryWatcherConf
, stopDirectoryWatcher
, listenToDir
, listenToTree
, onFileEvent
, readWatchedFile
, readWatchedFileLazy
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Lazy
import Data.Foldable (for_)
import Data.HashMap.Strict as HashMap
import Data.Watch
import System.Directory
import System.FSNotify

type GivenDirectoryWatcher = (?directoryWatcher :: DirectoryWatcher)

data DirectoryWatcher = DirectoryWatcher
  WatchManager
  (MVar (HashMap FilePath (Var RealWorld (Maybe Event), IOThunk (Maybe Event))))
  -- TODO: WeakRef? that would let us periodically prune the list of references if people stop caring

withDirectoryWatcher :: MonadUnliftIO m => (GivenDirectoryWatcher => m r) -> m r
withDirectoryWatcher k = withDirectoryWatcherConf defaultConfig k

withDirectoryWatcherConf :: MonadUnliftIO m => WatchConfig -> (GivenDirectoryWatcher => m r) -> m r
withDirectoryWatcherConf cfg k = withRunInIO \run ->
  withManagerConf cfg \ wm -> do
    w <- DirectoryWatcher wm <$> newMVar HashMap.empty
    let ?directoryWatcher = w
    run k

startDirectoryWatcher :: MonadIO m => m DirectoryWatcher
startDirectoryWatcher = liftIO do
  DirectoryWatcher <$> startManager <*> newMVar HashMap.empty

startDirectoryWatcherConf :: MonadIO m => WatchConfig -> m DirectoryWatcher
startDirectoryWatcherConf config = liftIO do
  DirectoryWatcher <$> startManagerConf config <*> newMVar HashMap.empty

stopDirectoryWatcher :: MonadIO m => DirectoryWatcher -> m ()
stopDirectoryWatcher (DirectoryWatcher wm _) = liftIO do stopManager wm

listenToDir :: (GivenDirectoryWatcher, MonadIO m) => FilePath -> m StopListening
listenToDir dir = case ?directoryWatcher of
  DirectoryWatcher wm paths -> liftIO do
    watchDir wm dir (const True) \e ->
      withMVar paths pure >>= \hm -> do
        canon <- canonicalizePath (eventPath e)
        for_ (HashMap.lookup canon hm) \(r,_) -> writeVar r (Just e)

listenToTree :: (GivenDirectoryWatcher, MonadIO m) => FilePath -> m StopListening
listenToTree dir = case ?directoryWatcher of
  DirectoryWatcher wm paths -> liftIO do
    watchTree wm dir (const True) \e ->
      withMVar paths pure >>= \hm -> do
        canon <- canonicalizePath (eventPath e)
        for_ (HashMap.lookup canon hm) \(r,_) -> writeVar r (Just e)

-- | This contains the last event associated with a given file. However, it only contains
-- events since someone started watching. Typical usecase is as a building block for
-- things that need the file content.
onFileEvent :: (GivenDirectoryWatcher, MonadIO m) => FilePath -> m (IOThunk (Maybe Event))
onFileEvent fp = case ?directoryWatcher of
  DirectoryWatcher _ fes -> liftIO do
    canon <- canonicalizePath fp
    modifyMVar fes \hm -> case HashMap.lookup canon hm of
      Just (_,t) -> pure (hm,t)
      Nothing -> do
        r <- newVar Nothing
        t <- delay (readVar r)
        pure (HashMap.insert canon (r,t) hm, t)

-- | invalidated every time the file changes so long as we are watching the containing directory
readWatchedFile :: (GivenDirectoryWatcher, MonadIO m) => FilePath -> m (IOThunk Strict.ByteString)
readWatchedFile fp = liftIO do
  e <- onFileEvent fp
  delay do
    _ <- force e
    liftIO do Strict.readFile fp

-- | invalidated every time the file changes so long as we are watching the containing directory
readWatchedFileLazy :: (GivenDirectoryWatcher,MonadIO m) => FilePath -> m (IOThunk Lazy.ByteString)
readWatchedFileLazy fp = liftIO do
  e <- onFileEvent fp
  delay do
    _ <- force e
    liftIO do Lazy.readFile fp
