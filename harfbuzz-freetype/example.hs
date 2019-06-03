{-# language OverloadedStrings #-}
{-# language StrictData #-}

import Control.Exception (displayException)
import Control.Monad.ST (RealWorld)
import Control.Monad
import Data.Atlas (Atlas)
import qualified Data.Atlas as Atlas
import Data.IORef
import Data.Foldable (for_)
import Data.StateVar
import Foreign.Marshal.Alloc
import GHC.Conc (setUncaughtExceptionHandler)
import Graphics.FreeType as FT
import Graphics.GL.Compatibility32
import Graphics.Glow
import Graphics.Harfbuzz
import Graphics.Harfbuzz.FreeType
import Linear
import qualified SDL
import System.Exit
import System.IO

data TextureAtlas = TextureAtlas 
  { ta_texture :: Texture
  , ta_atlas   :: Atlas RealWorld
  , ta_width   :: Int
  , ta_height  :: Int
  , ta_dirty   :: IORef Bool
  }

new_texture_atlas :: Int -> Int -> IO TextureAtlas
new_texture_atlas w h = do
  tex <- gen
  withBoundTexture Texture2D tex $ do
    texParameteri Texture2D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri Texture2D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
    atlas_data <- mallocBytes (w * h)
    glTexImage2D GL_TEXTURE_2D 0 GL_LUMINANCE (fromIntegral w) (fromIntegral h) 0 GL_LUMINANCE GL_UNSIGNED_BYTE atlas_data
    atlas <- Atlas.new w h Nothing
    TextureAtlas tex atlas w h <$> newIORef False

main :: IO ()
main = do
  setUncaughtExceptionHandler $ putStrLn . displayException
  hSetBuffering stdout NoBuffering

  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "harfbuzz-freetype example" SDL.defaultWindow
    { SDL.windowInitialSize = V2 640 480
    , SDL.windowHighDPI = True
    , SDL.windowOpenGL = Just SDL.defaultOpenGL
    }
  SDL.showWindow window
  _ <- SDL.glCreateContext window
  throwErrors

  library <- init_library
  face <- new_face library "test/fonts/Sanskrit2003.ttf" 0
  font <- hb_ft_font_create_referenced face

  ta <- new_texture_atlas 1024 1024

  forever $ do
    events <- SDL.pollEvents
    V2 w h <- SDL.glGetDrawableSize window
    glMatrixMode GL_PROJECTION -- old-school
    glOrtho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
    glMatrixMode GL_MODELVIEW
    glClear GL_COLOR_BUFFER_BIT
    withBoundTexture Texture2D (ta_texture ta) $ do
      glEnable GL_TEXTURE_2D
      glBegin GL_QUADS
      -- TODO: spew quads here, old school
      glEnd
      glDisable GL_TEXTURE_2D
    SDL.glSwapWindow window
    when (any escOrQuit events) $ do
      SDL.destroyWindow window
      SDL.quit
      exitSuccess

escOrQuit :: SDL.Event -> Bool
escOrQuit (SDL.Event _ e) = case e of
  SDL.QuitEvent -> True
  SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ SDL.KeycodeEscape _)) -> True
  _ -> False
