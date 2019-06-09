{-# language StrictData #-}
{-# language TypeFamilies #-}

-- | Usage sketch:
-- 
-- @
-- watchProgram :: DirectoryWatcher -> IO (Thunk Program)
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
-- main = withDirectoryWatcher $ \w -> do
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
module Data.Watch.Directory
( DirectoryWatcher(..)
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
import Control.Monad.Primitive
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Lazy
import Data.Foldable (for_)
import Data.HashMap.Strict as HashMap
import Data.Watch
import System.Directory
import System.FSNotify

data DirectoryWatcher = DirectoryWatcher
  WatchManager
  (MVar (HashMap FilePath (Ref RealWorld (Maybe Event), IOThunk (Maybe Event))))
  -- TODO: WeakRef? that would let us periodically prune the list of references if people stop caring

withDirectoryWatcher :: MonadIO m => (DirectoryWatcher -> IO r) -> m r
withDirectoryWatcher = withDirectoryWatcherConf defaultConfig

withDirectoryWatcherConf :: MonadIO m => WatchConfig -> (DirectoryWatcher -> IO r) -> m r
withDirectoryWatcherConf cfg k = liftIO $ withManagerConf cfg $ \wm -> newMVar HashMap.empty >>= k . DirectoryWatcher wm

startDirectoryWatcher :: MonadIO m => m DirectoryWatcher
startDirectoryWatcher = liftIO $ DirectoryWatcher <$> startManager <*> newMVar HashMap.empty

startDirectoryWatcherConf :: MonadIO m => WatchConfig -> m DirectoryWatcher
startDirectoryWatcherConf config = liftIO $ DirectoryWatcher <$> startManagerConf config <*> newMVar HashMap.empty

stopDirectoryWatcher :: MonadIO m => DirectoryWatcher -> m ()
stopDirectoryWatcher (DirectoryWatcher wm _) = liftIO $ stopManager wm

listenToDir :: MonadIO m => DirectoryWatcher -> FilePath -> m StopListening
listenToDir (DirectoryWatcher wm paths) dir = liftIO $
  watchDir wm dir (const True) $ \e ->
    withMVar paths pure >>= \hm -> do
      for_ (HashMap.lookup (eventPath e) hm) $ \(r,_) -> do
        writeRef r (Just e)
  
listenToTree :: MonadIO m => DirectoryWatcher -> FilePath -> m StopListening
listenToTree (DirectoryWatcher wm paths) dir = liftIO $ 
  watchTree wm dir (const True) $ \e ->
    withMVar paths pure >>= \hm -> do
      for_ (HashMap.lookup (eventPath e) hm) $ \(r,_) -> do
        writeRef r (Just e)
  
-- | This contains the last event associated with a given file. However, it only contains
-- events since someone started watching. Typical usecase is as a building
onFileEvent :: MonadIO m => DirectoryWatcher -> FilePath -> m (IOThunk (Maybe Event))
onFileEvent (DirectoryWatcher _ fes) fp = liftIO $ do
  afp <- makeAbsolute fp
  modifyMVar fes $ \hm -> case HashMap.lookup afp hm of
    Just (_,t) -> pure (hm,t)
    Nothing -> do
      r <- newRef Nothing
      t <- delay (readRef r)
      pure (HashMap.insert afp (r,t) hm, t)

-- | if you do this in a thunk the thunk will be invalidated every time the file changes
-- so long as we are watching the containing directory
readWatchedFile :: MonadIO m => DirectoryWatcher -> FilePath -> m (IOThunk Strict.ByteString)
readWatchedFile w fp = liftIO $ do
  e <- onFileEvent w fp
  delay $ do
    _ <- force e
    liftIO $ Strict.readFile fp

-- | if you do this in a thunk the thunk will be invalidated every time the file changes
-- so long as we are watching the containing directory
readWatchedFileLazy :: MonadIO m => DirectoryWatcher -> FilePath -> m (IOThunk Lazy.ByteString)
readWatchedFileLazy w fp = liftIO $ do
  e <- onFileEvent w fp
  delay $ do
    _ <- force e
    liftIO $ Lazy.readFile fp
