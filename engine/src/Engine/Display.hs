{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language MultiWayIf #-}
{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
{-# language StrictData #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Engine.Display 
  ( Display(..)
  , HasDisplay(..)
  , warn
  , newDisplay
  , resizeDisplay
  , handleDisplayEvents
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict hiding (get)
import Control.Lens
import Data.Text
import Graphics.GL.Core41
import Linear
import SDL hiding (Display)
import System.IO

import Engine.Exception
import Engine.SDL

-- | Basic OpenGL + SDL display that provides the screen and various bits of metadata about the act of displaying data
data Display = Display 
  { _displayWindow :: Window
  , _displayGL :: GLContext
  , _displayWindowSize :: V2 Int
  , _displayFullScreen
  , _displayWindowSizeChanged
  , _displayMinimized
  , _displayHasMouseFocus
  , _displayHasKeyboardFocus
  , _displayVisible
  , _displayWantsRelativeMouseMode :: Bool
  }

makeClassy ''Display

newDisplay :: IO (SDL.Window, MVar Display)
newDisplay = do 
  SDL.initialize [SDL.InitVideo]
  let initialSize = V2 640 480
  window <- SDL.createWindow "engine example" SDL.defaultWindow
    { SDL.windowInitialSize = fromIntegral <$> initialSize
    , SDL.windowHighDPI = True
    , SDL.windowResizable = True
    , SDL.windowInputGrabbed = False
    , SDL.windowOpenGL = Just SDL.defaultOpenGL
      { SDL.glProfile = SDL.Core SDL.Debug 4 1
      , SDL.glColorPrecision = V4 8 8 8 0
      , SDL.glStencilPrecision = 8
      , SDL.glDepthPrecision = 24
      , SDL.glMultisampleSamples = 16
      }
    }
  SDL.showWindow window
  context <- SDL.glCreateContext window
  md <- newMVar $ Display window context initialSize False True False True True True False
  pure (window, md) -- kinda janky but nicer to use

type GivenDisplay = (?display :: MVar Display)

-- | Complain loudly enough to pop up a window
warn :: (GivenDisplay, MonadIO m) => Text -> Text -> m ()
warn title message = do
  liftIO $ do 
    hPutStrLn stderr $ "Warning: " ++ unpack title ++ ": " ++ unpack message
    withMVar ?display $ \d -> showSimpleMessageBox (Just $ _displayWindow d) Warning title message

-- | Recalculate the actual OpenGL viewport size considering Retina
resizeDisplay :: (GivenDisplay, MonadIO m) => m ()
resizeDisplay = liftIO $ modifyMVar_ ?display $ \d -> do
  V2 w h <- SDL.glGetDrawableSize (_displayWindow d )
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  pure $ d & displayWindowSize .~ V2 (fromIntegral w) (fromIntegral h)

-- | Discharge events we should always handle correctly, e.g. CUA concerns for quitting, going full-screen, etc.
handleDisplayEvents :: (GivenDisplay, MonadIO m) => [Event] -> m ()
handleDisplayEvents events = liftIO $ modifyMVar_ ?display $ execStateT (traverse handleDisplayEvent events)

handleDisplayEvent :: Event -> StateT Display IO ()
handleDisplayEvent (Event _timestamp e) = case e of
  QuitEvent{} -> shutdown
  WindowClosedEvent{} -> shutdown
  WindowLostKeyboardFocusEvent{} -> releaseMouse
  WindowGainedKeyboardFocusEvent{} -> takeMouse
  WindowLostMouseFocusEvent{} -> do
    displayHasMouseFocus .= False
    releaseMouse
  WindowGainedMouseFocusEvent{} -> do
    displayHasMouseFocus .= True
    takeMouse 
  WindowExposedEvent{} -> do
    displayVisible .= True
  WindowHiddenEvent{} -> do
    displayVisible .= False
    displayHasKeyboardFocus .= False
    displayHasMouseFocus .= False
    releaseMouse
  WindowMovedEvent{} -> pure ()
  WindowSizeChangedEvent (WindowSizeChangedEventData _ (V2 w h)) -> do
    displayWindowSize .= V2 (fromIntegral w) (fromIntegral h)
    displayWindowSizeChanged .= True
  WindowResizedEvent (WindowResizedEventData _ (V2 w h)) -> do
    displayWindowSize .= V2 (fromIntegral w) (fromIntegral h)
    displayWindowSizeChanged .= True
  WindowMinimizedEvent{} -> do
    displayHasKeyboardFocus .= False
    displayHasMouseFocus    .= False
    displayVisible          .= False
    releaseMouse
  WindowMaximizedEvent{} -> do
    displayHasKeyboardFocus .= True
    displayHasMouseFocus    .= True
    displayVisible          .= True
    takeMouse
  WindowRestoredEvent{} -> do
    displayVisible          .= True -- unminimized
    displayHasKeyboardFocus .= True
    takeMouse
  WindowShownEvent{} ->  do
    displayHasKeyboardFocus .= True
    displayVisible          .= True
    takeMouse
  KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ k KeyModifier {..})) 
    | k == KeycodeQ, keyModifierLeftGUI || keyModifierRightGUI -> shutdown
    | k == KeycodeEscape -> releaseMouse
    | k == KeycodeReturn, keyModifierLeftGUI || keyModifierRightGUI -> do
      fs <- displayFullScreen <%= not
      -- fsn <- view optionsFullScreenNormal, use actual full screen w/ resolution change and everything like it is 1999
      w <- use displayWindow
      setWindowMode w $ if
        | not fs -> Windowed
        -- | fsn -> Fullscreen
        | otherwise -> FullscreenDesktop
      takeMouse
    | otherwise -> takeMouse
  MouseButtonEvent{} -> takeMouse
  _ -> pure () -- liftIO $ hPrint stderr e
 where
  takeMouse    = (relativeMouseMode $=) =<< use displayWantsRelativeMouseMode
  releaseMouse = relativeMouseMode $= False
