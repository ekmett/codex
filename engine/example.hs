{-# language OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# language StrictData #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}
{-# language TupleSections #-}
{-# language ImplicitParams #-}
{-# language TemplateHaskell #-}

import Control.Exception
import Control.Exception.Lens
--import Control.Lens
import Control.Monad
import Data.Default
import Data.IORef
import GHC.Conc (setUncaughtExceptionHandler)
--import Graphics.GL.Core41
--import Graphics.Glow
--import Linear
import qualified SDL
import System.IO
--import Data.Watch
import Data.Watch.Directory

import Engine.Display
import Engine.Exception
import Engine.Input
import Engine.Task

-- A fairly minimal old-school fixed function pipeline demo
main :: IO ()
main = withDirectoryWatcher $ withTasks $ \pumpTasks -> do
  setUncaughtExceptionHandler $ putStrLn . displayException
  hSetBuffering stdout NoBuffering
  (window, _display) <- newDisplay; let ?display = _display
  _input <- newIORef def; let ?input = _input
  _ <- listenToTree "shaders"
  --namedStrings <- loadIncludes "shaders"

  -- use trying _Shutdown
  _ <- trying _Shutdown $ forever $ do
    events <- SDL.pollEvents
    handleInputEvents events
    handleDisplayEvents events
    SDL.glSwapWindow window
    pumpTasks

  SDL.destroyWindow window
  SDL.quit
