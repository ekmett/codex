{-# language OverloadedStrings #-}
{-# language StrictData #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

import Control.Lens
import Control.Exception (displayException)
import Control.Monad.ST (RealWorld)
import Control.Monad
import Data.Atlas (Atlas,Box(..),Pt(..))
import qualified Data.Atlas as Atlas
import Data.Map (Map)
import Data.IORef
import Data.Foldable (for_)
import Data.Functor.Identity
import Data.StateVar
import Data.Vector.Generic.Lens
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Ptr.Diff
import GHC.Conc (setUncaughtExceptionHandler)
import Graphics.FreeType as FT
import Graphics.GL.Compatibility32
import Graphics.Glow
import Graphics.Harfbuzz hiding (Map)
import Graphics.Harfbuzz.FreeType
import Linear
import qualified SDL
import System.Exit
import System.IO

data TextureAtlas = TextureAtlas
  { ta_texture :: Texture
  , ta_atlas   :: Atlas RealWorld
  , ta_buffer  :: Ptr ()
  , ta_width   :: Int
  , ta_height  :: Int
  , ta_dirty   :: IORef Bool
  , ta_glyphs  :: IORef (Map (FT.Face,Codepoint) TextureGlyph)
  }

data TextureGlyph = TextureGlyph
  { tg_w, tg_h, tg_x, tg_y, tg_bx, tg_by :: Int
  } deriving (Eq,Show)

find_glyph :: TextureAtlas -> FT.Face -> Codepoint -> IO TextureGlyph
find_glyph TextureAtlas{..} face codepoint = do
  gs <- readIORef ta_glyphs
  case gs^.at (face,codepoint) of
    Just tg -> return tg
    Nothing -> do
      load_glyph face codepoint LOAD_RENDER
      glyphslot <- face_glyph face
      withForeignPtr glyphslot $ \slot -> do
        Bitmap{..} <- peekDiff p glyphslot_bitmap
        let w = fromIntegral bitmap_width
            h = fromIntegral bitmap_rows
        bx <- peekDiff slot glyphslot_bearing_x
        by <- peekDiff slot glyphslot_bearing_x
        pack ta_atlas id (const id) (const id) (Identity (boxy (Pt w h))) >>= \case
          Right (Box (Identity (Pt x y)) _) -> do
            let tg = TextureGlyph bitmap_width bitmap_rows x y bx by
            writeIORef $ gs & at (face,codepoint) ?~ tg
            let p = fromIntegral bitmap_pitch
            -- now to copy
            bmp <- if p == w
              then pure bitmap_data -- we can use it directly
              else ta_buffer <$ do for [0..h-1] $ \ y -> copyBytes (plusPtr ta_buffer (y*w)) (plusPtr src (y*p)) width -- copy line by line
            tg <$ glTexSubImage2D GL_TEXTURE_2D 0 x y w h GL_RED GL_UNSIGNED_BYTE bmp
          Left _ -> error "out of room" -- TODO: empty and retry

new_texture_atlas :: Int -> Int -> IO TextureAtlas
new_texture_atlas w h = do
  tex <- gen
  withBoundTexture Texture2D tex $ do
    texParameteri Texture2D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri Texture2D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
    glPixelStorei GL_PACK_ALIGNMENT 1
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    atlas_data <- mallocBytes (w * h) -- used as a scratch buffer for texture uploads  w/ glTexSubImage2D
    glTexImage2D GL_TEXTURE_2D 0 GL_LUMINANCE (fromIntegral w) (fromIntegral h) 0 GL_LUMINANCE GL_UNSIGNED_BYTE atlas_data
    atlas <- Atlas.new w h Nothing
    TextureAtlas tex atlas atlas_data w h <$> newIORef False <*> newIORef mempty

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
  set_pixel_sizes face 0 32
  font <- hb_ft_font_create face

  ta <- new_texture_atlas 1024 1024

  buffer <- buffer_create
  buffer_direction buffer $= DIRECTION_LTR
  buffer_language buffer $= "hi"
  buffer_script buffer $= SCRIPT_DEVANAGARI
  let text = "हालाँकि प्रचलित रूप पूजा"
  buffer_add_text buffer text 0 (lengthWord16 text)
  shape font buffer mempty

  bitmap <- act glyphslot_bitmap <$> face_glyph face

  forever $ do
    events <- SDL.pollEvents
    V2 w h <- SDL.glGetDrawableSize window
    glMatrixMode GL_PROJECTION -- old-school
    glOrtho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
    glMatrixMode GL_MODELVIEW
    glClear GL_COLOR_BUFFER_BIT
    withBoundTexture Texture2D (ta_texture ta) $ do
      glEnable GL_TEXTURE_2D
      gis <- buffer_get_glyph_infos buffer
      withForeignPtr bitmap $ \p -> do
        iforOf each gis $ \i (glyph_info_codepoint -> g) -> do
          TextureGlyph{..} <- find_glyph ta face g
          let w = fromIntegral tg_w / fromIntegral ta_width
              x = fromIntegral tg_x / fromIntegral ta_width
              h = fromIntegral tg_h / fromIntegral ta_height
              y = fromIntegral tg_y / fromIntegral ta_height
          glBegin GL_QUADS
          --glTexCoord2f x     y;     glVertex(...);
          --glTexCoord2f (x+w) y;     glVertex(...);
          --glTexCoord2f (x+w) (y+h); glVertex(...);
          --glTexCoord2f x     (y+h); glVertex(...);
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
