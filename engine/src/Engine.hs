{-# language ImplicitParams #-}
{-# language RankNTypes #-}
{-# language ConstraintKinds #-}
{-# language BlockArguments #-}
-- |
-- Usage:
--
-- @
-- main = withEngine $ \drive ->
--   bracket setupResources teardownResources $ \resources -> drive $ do
--     ... use resources here to draw a frame

module Engine
( GivenEvents
, withEngine
) where

import Control.Exception
import Control.Exception.Lens
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Default
import Data.IORef
import Data.Text
import Data.Watch.Directory
import Engine.Exception
import Engine.Meter
import Engine.SDL
import Engine.Shader.Include
import Engine.Task
import Engine.Time
import GHC.Conc (setUncaughtExceptionHandler)
import Numeric
import SDL
import System.Directory
import System.IO

updateFPS :: GivenWindow => IORef Meter -> IO ()
updateFPS meter = do
  t <- now
  m <- liftIO $ atomicModifyIORef meter \m -> let m' = tick t m in (m',m')
  windowTitle ?window $= pack (showString "(fps: " $ showFFloat (Just 1) (fps m) ")")

type GivenSetupInfo = (GivenShaderDir, GivenIncludeCache, GivenWindow)
type GivenFrameInfo = (GivenInput, GivenEvents)

-- TODO: Option parsing
withEngine :: MonadUnliftIO m => (GivenSetupInfo => ((GivenFrameInfo => m a) -> m ()) -> m ()) -> m ()
withEngine k = withRunInIO \run1 -> do
  shaderDir <- canonicalizePath "shaders" -- for now
  let ?shaderDir = shaderDir
  liftIO do
    setUncaughtExceptionHandler $ putStrLn . displayException
    hSetBuffering stdout NoBuffering
  meter <- newIORef def
  withDirectoryWatcher $ withTasks \pumpTasks -> do
    stopListeningToShaderDir <- listenToTree shaderDir
    withWindow do
      inputRef <- newIORef def
      withIncludeCache do
        -- user setup 
        run1 $ k \draw -> withRunInIO \run2 -> do
          _ <- trying _Shutdown $ forever do
            _events <- SDL.pollEvents; let ?events = _events
            handleWindowEvents
            _input <- atomicModifyIORef inputRef $ join (,) . handleInputEvents
            let ?input = _input
            _ <- run2 draw
            SDL.glSwapWindow ?window
            updateFPS meter
            pumpTasks
          pure ()
        -- user teardown
    stopListeningToShaderDir
