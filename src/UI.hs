{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett and Sean Chalmers
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module UI
  ( main
  , UI(..)
  , HasUI(..)
  ) where

import Control.Lens (view)
import Control.Lens.TH (makeClassy)

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.StateVar
import Foreign.C.Types (CInt)
import Graphics.GL
import Linear (V2 (..))
import Linear.Affine (_Point)
import qualified SDL

import Graphics.Glow
import UI.Shaders

data UI = UI
  { _uiProgram  :: Program
  , _uiMouseUni :: UniformLocation
  }
makeClassy ''UI

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

initResources :: ByteString -> ByteString -> IO UI
initResources fsSource vsSource = do
  vs <- buildShaderFrom VertexShader vsSource
  fs <- buildShaderFrom FragmentShader fsSource
  p <- buildProgram vs fs
  UI p <$> uniformLocation p "iMouse"

escOrQuit :: SDL.Event -> Bool
escOrQuit (SDL.Event _ evt) = isAQuit evt where
  isAQuit = \case
    SDL.QuitEvent                                                                      -> True
    SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ SDL.KeycodeEscape _)) -> True
    _                                                                                  -> False

draw :: SDL.Window -> UI -> SDL.Point V2 CInt -> IO ()
draw window (UI prog imouse) mouseLoc = do
  glClear GL_COLOR_BUFFER_BIT
  V2 w h <- get (SDL.windowSize window) -- allow for HighDPI displays and resizing
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  currentProgram $= prog
  programUniform2f prog imouse $= fromIntegral <$> view _Point mouseLoc
  glDrawArrays GL_TRIANGLES 0 3

main :: IO ()
main = do
  fragSrc <- shader "shaders/basic_box.frag"
  vertSrc <- shader "shaders/one_triangle.vert"

  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "SDF UI Element Testing" SDL.defaultWindow
    { SDL.windowInitialSize = V2 screenWidth screenHeight
    , SDL.windowHighDPI = True
    , SDL.windowInputGrabbed = True
    , SDL.windowOpenGL = Just SDL.defaultOpenGL
      { SDL.glProfile = SDL.Core SDL.Debug 4 1
      }
    }

  SDL.cursorVisible $= False
  SDL.showWindow window
  _ <- SDL.glCreateContext window

  -- we should have one global one of these for the entire application
  emptyVAO <- gen
  boundVertexArray $= emptyVAO

  prog <- initResources fragSrc vertSrc

  throwErrors

  let loop = do
        events <- SDL.pollEvents

        mouseEvt <- SDL.getAbsoluteMouseLocation

        glClear GL_COLOR
        draw window prog mouseEvt
        SDL.glSwapWindow window

        unless (any escOrQuit events) loop

  loop
  SDL.destroyWindow window
  SDL.quit
