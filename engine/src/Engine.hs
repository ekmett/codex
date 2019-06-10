{-# language ImplicitParams #-}
{-# language RankNTypes #-}
module Engine
( withEngine
) where

import Control.Exception
import Control.Exception.Lens
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Default
import Data.IORef
import Data.Watch.Directory
import Engine.Display
import Engine.Exception
import Engine.Input
import Engine.Task
import GHC.Conc (setUncaughtExceptionHandler)
import SDL
import System.IO

withEngine :: MonadUnliftIO m => ((GivenDirectoryWatcher, GivenInput, GivenDisplay) => (m a -> m ()) -> m ()) -> m ()
withEngine k = withRunInIO $ \run -> do
  liftIO $ do
    setUncaughtExceptionHandler $ putStrLn . displayException
    hSetBuffering stdout NoBuffering
  withDirectoryWatcher $ withTasks $ \pumpTasks -> do
    (window, _display) <- newDisplay; let ?display = _display
    _input <- liftIO $ newIORef def; let ?input = _input
    _ <- listenToTree "shaders"
    run $ k $ \draw -> withRunInIO $ \run2 -> do
      _ <- trying _Shutdown $ run2 $ forever $ do
        events <- SDL.pollEvents
        handleInputEvents events
        handleDisplayEvents events
        _ <- draw
        SDL.glSwapWindow window
        liftIO $ pumpTasks
      pure ()

    SDL.destroyWindow window
    SDL.quit
