{-# language TemplateHaskell #-}
{-# language ConstraintKinds #-}
{-# language ImplicitParams #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}
{-# language StrictData #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Do we need to hook window entry to get a better state summary of the mouse buttons to avoid the common 'sticky mouse' problems in games?
--
-- Should losing focus kill the mouse button states?
--------------------------------------------------------------------
module Engine.Input
  ( Input(..)
  , HasInput(..)
  , GivenInput
  , handleInputEvents
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Foldable
import Data.Int
import Data.IORef
import Data.StateVar
import Data.Map as Map
import Data.Word
import SDL.Event
import SDL.Input.Keyboard
import SDL.Input.Mouse
import Linear
import Linear.Affine

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

type GivenInput = (?input :: IORef Input)

handleInputEvents :: (MonadIO m, GivenInput) => [Event] -> m ()
handleInputEvents events = ?input $~! execState (traverse_ handleInputEvent events) 
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
