{-# language ConstraintKinds #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language StrictData #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
{-# language BlockArguments #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A grab-bag of stuff for working with SDL2
module Engine.SDL
( AsSDLException(..)
, GivenEvents
, windowState
-- * Convenience
, GivenWindow
, warn
, withWindow
, resizeViewport
, handleWindowEvents
, Input(..)
, HasInput(..)
, GivenInput
, handleInputEvents
) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.State.Strict hiding (get)
import Data.Default
import Data.Foldable
import Data.Int
import Data.Map as Map
import Data.StateVar
import Data.Text
import Data.Word
import Engine.Exception
import Graphics.GL.Core41
import Linear
import Linear.Affine
import SDL
import SDL.Internal.Numbered
import SDL.Internal.Types
import qualified SDL.Raw as Raw
import System.IO

--------------------------------------------------------------------------------
-- * Missing bits of the SDL2 
--------------------------------------------------------------------------------

makeClassyPrisms ''SDLException
instance AsSDLException SomeException where _SDLException = exception

windowState :: SDL.Window -> StateVar WindowMode
windowState w@(Window rw) = StateVar getWindowMode (setWindowMode w) where
  getWindowMode = fromNumber <$> Raw.getWindowFlags rw

--------------------------------------------------------------------------------
-- * Window management
--------------------------------------------------------------------------------

type GivenEvents = ?events :: [SDL.Event]
type GivenWindow = ?window :: Window

withWindow :: MonadUnliftIO m => (GivenWindow => m a) -> m a
withWindow k = withRunInIO \io -> bracket create destroy \w -> let ?window = w in io k where
  create = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "engine example" SDL.defaultWindow
      { SDL.windowInitialSize = V2 640 480
      , SDL.windowHighDPI = True
      , SDL.windowResizable = True
      , SDL.windowInputGrabbed = False
      , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Debug 4 1
        , SDL.glColorPrecision = V4 8 8 8 0
        , SDL.glStencilPrecision = 8
        , SDL.glDepthPrecision = 24
        , SDL.glMultisampleSamples = 16
        }
      }
    SDL.showWindow window
    window <$ SDL.glCreateContext window
  destroy w = do
    SDL.destroyWindow w
    SDL.quit

-- | Complain loudly enough to pop up a window
warn :: (GivenWindow, MonadIO m) => Text -> Text -> m ()
warn title message = liftIO do
  hPutStrLn stderr $ "Warning: " ++ unpack title ++ ": " ++ unpack message
  showSimpleMessageBox (Just ?window) Warning title message

-- | Recalculate the actual OpenGL viewport size considering Retina, TODO: include whether or not we're visible
resizeViewport :: (GivenWindow, MonadIO m) => m ()
resizeViewport = liftIO do
  V2 w h <- SDL.glGetDrawableSize ?window
  glViewport 0 0 (fromIntegral w) (fromIntegral h)

-- | Discharge events we should always handle correctly, e.g. CUA concerns for quitting, going full-screen, etc.
handleWindowEvents :: (MonadIO m, GivenWindow, GivenEvents) => m ()
handleWindowEvents = liftIO $ for_ ?events handleWindowEvent

handleWindowEvent :: GivenWindow => Event -> IO ()
handleWindowEvent (Event _timestamp e) = case e of
  QuitEvent{}         -> shutdown
  WindowClosedEvent{} -> shutdown
  KeyboardEvent (KeyboardEventData _ Pressed rep (Keysym _ k KeyModifier{..}))
    | k == KeycodeQ, gui -> shutdown
    | k == KeycodeReturn, gui, not rep ->
      windowState (?window) $~! \case
        SDL.FullscreenDesktop -> SDL.Windowed
        SDL.Fullscreen -> SDL.Windowed
        SDL.Windowed -> SDL.FullscreenDesktop -- TODO: use an option to pick this or FullScreen
        SDL.Maximized -> SDL.FullscreenDesktop
        SDL.Minimized -> SDL.FullscreenDesktop
    where gui = keyModifierLeftGUI || keyModifierRightGUI
  _ -> pure ()

--------------------------------------------------------------------------------
-- * Input management
--------------------------------------------------------------------------------

data Input = Input
  { _keyCodes        :: Map Keycode (Bool,KeyModifier) -- Bool contains repeat info
  , _scanCodes       :: Map Scancode (Bool,KeyModifier)
  , _mouseButtons    :: Map MouseButton Word8
  , _mousePos        :: Point V2 Int32
  , _mouseButtonMask :: [MouseButton] -- for motion
  , _mouseRel        :: V2 Int32 -- relative since last reset
  , _mouseWheel      :: V2 Int32 -- relative since last reset
  } deriving Show

makeClassy ''Input

instance Default Input where
  def = Input def def def 0 [] 0 0

-- | This provides convenient access to user events without actually processing them yourself
--
-- If you need more information, use 'GivenEvents' and process them yourself each frame.
type GivenInput = (?input :: Input)

handleInputEvents :: GivenEvents => Input -> Input
handleInputEvents = execState (traverse_ handleInputEvent ?events)
{-# inline handleInputEvents #-}

handleInputEvent :: Event -> State Input ()
handleInputEvent (Event _ p) = handleInputEventPayload p

handleInputEventPayload :: EventPayload -> State Input ()
handleInputEventPayload = \case
  KeyboardEvent (KeyboardEventData _ action rep (Keysym sc kc km)) -> do
    let s = (rep,km) <$ guard (action == Pressed)
    keyCodes.at kc .= s
    scanCodes.at sc .= s
  MouseMotionEvent (MouseMotionEventData _ Mouse{} buttons pos rel) -> do
    mousePos .= pos
    mouseButtonMask .= buttons
    mouseRel += rel
  MouseButtonEvent (MouseButtonEventData _ action Mouse{} button clicks pos) -> do
    mousePos .= pos
    mouseButtons.at button .= case action of
      Pressed -> Just clicks
      Released -> Nothing
  MouseWheelEvent (MouseWheelEventData _ Mouse{} (V2 x y) dir) -> do
    mouseWheel += V2 x (if dir == ScrollFlipped then negate y else y)
  _ -> return ()
