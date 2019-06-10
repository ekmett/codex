{-# language ImplicitParams #-}
{-# language RankNTypes #-}
module Engine
( withEngine
) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Exception.Lens
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Default
import Data.IORef
import Data.Text
import Data.Watch.Directory
import Engine.Display
import Engine.Exception
import Engine.Meter
import Engine.Input
import Engine.Task
import Engine.Time
import GHC.Conc (setUncaughtExceptionHandler)
import Numeric
import SDL
import System.IO

updateFPS :: GivenDisplay => IORef Meter -> IO ()
updateFPS meter = do
  t <- now
  m <- liftIO $ atomicModifyIORef meter $ \m -> let m' = tick t m in (m',m')
  withMVar (?display) $ \d -> do
    windowTitle (_displayWindow d) $= pack (showString "engine (fps: " $ showFFloat (Just 1) (fps m) ")") 

withEngine :: MonadUnliftIO m => ((GivenDirectoryWatcher, GivenInput, GivenDisplay) => (m a -> m ()) -> m ()) -> m ()
withEngine k = withRunInIO $ \run1 -> do
  liftIO $ do
    setUncaughtExceptionHandler $ putStrLn . displayException
    hSetBuffering stdout NoBuffering
  meter <- newIORef def
  withDirectoryWatcher $ withTasks $ \pumpTasks -> do
    (window, _display) <- newDisplay; let ?display = _display
    _input <- liftIO $ newIORef def; let ?input = _input
    _ <- listenToTree "shaders"
    run1 $ k $ \draw -> withRunInIO $ \run2 -> do
      _ <- trying _Shutdown $ forever $ do
        events <- SDL.pollEvents
        handleInputEvents events
        handleDisplayEvents events
        _ <- run2 draw
        SDL.glSwapWindow window
        updateFPS meter
        pumpTasks
      pure ()

    SDL.destroyWindow window
    SDL.quit
