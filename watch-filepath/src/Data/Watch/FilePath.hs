{-# language StrictData #-}
{-# language TypeFamilies #-}

-- | Usage sketch:
-- 
-- @
-- watchProgram :: FileWatcher -> IO (Thunk Program)
-- watchProgram w = do
--   vthunk <- delayWithIO delete $ readWatchedFileLazy w "shaders/foo.vert" >>= compileShader VertexShader
--   fthunk <- delayWithIO delete $ readWatchedFileLazy w "shaders/foo.frag" >>= compileShader FragmentShader
--   delayWithIO delete $ do
--     vs <- force vthunk
--     fs <- force fthunk
--     glAttachShader p vs
--     glAttachShader p fs
--     glLinkProgram p
--
-- main = withFileWatcher $ \w -> do
--   listenToTree w "shaders"
--   ...
--   foo <- watchProgram w
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
module Data.Watch.FilePath
( FileWatcher(..)
, withFileWatcher
, withFileWatcherConf
, startFileWatcher
, startFileWatcherConf
, stopFileWatcher
, listenToDir
, listenToTree
, onFileEvent
, readWatchedFile
, readWatchedFileLazy
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Primitive
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Lazy
import Data.Foldable (for_)
import Data.HashMap.Strict as HashMap
import Data.Watch
import System.FSNotify

data FileWatcher = FileWatcher
  WatchManager
  (MVar (HashMap FilePath (Ref RealWorld (Maybe Event), IOThunk (Maybe Event))))
  -- TODO: WeakRef?

withFileWatcher :: MonadIO m => (FileWatcher -> IO r) -> m r
withFileWatcher = withFileWatcherConf defaultConfig

withFileWatcherConf :: MonadIO m => WatchConfig -> (FileWatcher -> IO r) -> m r
withFileWatcherConf cfg k = liftIO $ withManagerConf cfg $ \wm -> newMVar HashMap.empty >>= k . FileWatcher wm

startFileWatcher :: MonadIO m => m FileWatcher
startFileWatcher = liftIO $ FileWatcher <$> startManager <*> newMVar HashMap.empty

startFileWatcherConf :: MonadIO m => WatchConfig -> m FileWatcher
startFileWatcherConf config = liftIO $ FileWatcher <$> startManagerConf config <*> newMVar HashMap.empty

stopFileWatcher :: MonadIO m => FileWatcher -> m ()
stopFileWatcher (FileWatcher wm _) = liftIO $ stopManager wm

listenToDir :: MonadIO m => FileWatcher -> FilePath -> m StopListening
listenToDir (FileWatcher wm paths) dir = liftIO $
  watchDir wm dir (const True) $ \e ->
    withMVar paths pure >>= \hm ->
      for_ (HashMap.lookup (eventPath e) hm) $ \(r,_) ->
        writeRef r (Just e)
  
listenToTree :: MonadIO m => FileWatcher -> FilePath -> m StopListening
listenToTree (FileWatcher wm paths) dir = liftIO $ 
  watchTree wm dir (const True) $ \e ->
    withMVar paths pure >>= \hm ->
      for_ (HashMap.lookup (eventPath e) hm) $ \(r,_) ->
        writeRef r (Just e)
  
-- | This contains the last event associated with a given file. However, it only contains
-- events since someone started watching. Typical usecase is as a building
onFileEvent :: MonadIO m => FileWatcher -> FilePath -> m (IOThunk (Maybe Event))
onFileEvent (FileWatcher _ fes) fp = liftIO $ do
  modifyMVar fes $ \hm -> case HashMap.lookup fp hm of
    Just (_,t) -> pure (hm,t)
    Nothing -> do
      r <- newRef Nothing
      t <- delay (readRef r)
      pure (HashMap.insert fp (r,t) hm, t)

-- | if you do this in a thunk the thunk will be invalidated every time the file changes
-- so long as we are watching the containing directory
readWatchedFile :: MonadIO m => FileWatcher -> FilePath -> m (IOThunk Strict.ByteString)
readWatchedFile w fp = liftIO $ do
  e <- onFileEvent w fp
  delay $ do
    _ <- force e
    liftIO $ Strict.readFile fp

-- | if you do this in a thunk the thunk will be invalidated every time the file changes
-- so long as we are watching the containing directory
readWatchedFileLazy :: MonadIO m => FileWatcher -> FilePath -> m (IOThunk Lazy.ByteString)
readWatchedFileLazy w fp = liftIO $ do
  e <- onFileEvent w fp
  delay $ do
    _ <- force e
    liftIO $ Lazy.readFile fp
