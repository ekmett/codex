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
import Data.Text.Foreign (lengthWord16)
import Data.Foldable (for_)
import Data.Functor.Identity
import Data.Proxy
import Data.StateVar
import Data.Vector.Generic.Lens
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
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

data TextureAtlas k = TextureAtlas
  { ta_texture :: Texture
  , ta_atlas   :: Atlas RealWorld
  , ta_buffer  :: Ptr ()
  , ta_width   :: Int
  , ta_height  :: Int
  , ta_dirty   :: IORef Bool
  , ta_glyphs  :: IORef (Map k TextureGlyph)
  }

data TextureGlyph = TextureGlyph
  { tg_w, tg_h, tg_x, tg_y, tg_bx, tg_by :: Int
  } deriving (Eq,Show)

-- | Assumes GL_TEXTURE_2D is bound to the atlas texture and that scratch is large enough to handle the bitmap glyph
upload :: BitmapGlyph -> Pt -> Ptr () -> IO TextureGlyph
upload bg (Pt x y) scratch = do
  withForeignPtr bg $ \p ->
    Bitmap{..} <- _bitmapglyph_bitmap
    let w = fromIntegral bitmap_width
        h = fromIntegral bitmap_rows
        p = fromIntegral bitmap_pitch
    target <- if w == p
      then pure bitmap_buffer -- usable directly
      else scratch <$ do for_ [0..h-1] $ \ y -> copyBytes (plusPtr ta_buffer (y*w)) (plusPtr bitmap_buffer (y*p)) w -- messy, pack into scratch
    glTexSubImage2D GL_TEXTURE_2D 0 x y w h GL_RED GL_UNSIGNED_BYTE target
    bx <- fromIntegral <$> peekDiff p _bitmapglyph_left
    by <- fromIntegral <$> peekDiff p _bitmapglyph_left
    pure $ TextureGlyph w h x y bx by
  
-- | render a single glyph
render :: (FT.Face, Codepoint) -> IO BitmapGlyph
render (face,codepoint) = do
  load_glyph face codepoint LOAD_RENDER
  glyphslot <- face_glyph face
  glyph <- get_glyph glyphslot
  glyph_to_bitmap glyph RENDER_MODE_NORMAL def False

size :: BitmapGlyph -> IO Pt
size bmg = withForeignPtr (act _bitmap_glyph_bitmap bmg) $ \bp ->
    Pt <$> do fromIntegral <$> peekDiff bp _bitmap_glyph_width
       <*> do fromIntegral <$> peekDiff bp _btimap_glyph_rows

-- | Assumes Texture atlas is currently bound to GL_TEXTURE_2D
-- This should batch the requests using @Atlas.pack@, trying to cache an entire list of codepoints
-- and taking a many as it can get.
batch
  :: (Traversable f, Ord k)
  => TextureAtlas k
  -> (a -> k) -- feature extraction
  -> (k -> (FT.Face,Codepoint))
  -> (a -> TextureGlyph -> IO ()) -- callback
  -> [a]
  -> IO ()
batch TextureAtlas{..} key keyinfo callback as = do
  known <- readIORef ta_glyphs -- known :: Map k TextureGlyph
  distinct <- toList <$> setOf (traverse.to key.filter (\k -> hasn't (ix k) known)) as -- distinct :: [k]
  fresh <- for distinct $ \k -> (k,) <$> render (info k) -- fresh :: [(k,BitmapGlyph)]
  Atlas.pack ta_atlas (size.snd) (,) (,) ks >>= \case
    Right xs -> do -- xs :: [((k,BitmapGlyph),Pt)]
      uploaded <- do for xs $ \((k,bmg),xy) -> (k,) <$> upload bmg xy ta_buffer
      writeIORef ta_glyphs $ known <> Map.fromList uploaded
      for ks $ 
      traverse (uncurry callback) uploaded
    Left ys -> do -- ys :: [((k,BitmapGlyph),Maybe Pt)]
      uploaded <- do for xs $ \(k,bm
       
      
      
  
    

  
  
  gs <- readIORef ta_glyphs
  case gs^.at (face,codepoint) of
    Just tg -> return tg
    Nothing -> do
      load_glyph face codepoint LOAD_RENDER
      glyphslot <- face_glyph face
      withForeignPtr glyphslot $ \slot -> do
        Bitmap{..} <- peekDiffOff slot glyphslot_bitmap
        let w = fromIntegral bitmap_width
            h = fromIntegral bitmap_rows
        bx <- fromIntegral <$> peekDiffOff slot glyphslot_bitmap_left
        by <- fromIntegral <$> peekDiffOff slot glyphslot_bitmap_top
        Atlas.pack1 ta_atlas (Pt w h) >>= \case
          Just (Pt x y) -> do
            let tg = TextureGlyph (fromIntegral bitmap_width) (fromIntegral bitmap_rows) x y bx by
            writeIORef $ gs & at (face,codepoint) ?~ tg
            -- copy, relatively unnecessary for now, but when we batch these up, it will be
            for_ [0..h-1] $ \ y -> copyBytes (plusPtr ta_buffer (y*w)) (plusPtr bitmap_buffer (y*p)) w -- pack line by line into the scratch pad
            tg <$ glTexSubImage2D GL_TEXTURE_2D 0 x y w h GL_RED GL_UNSIGNED_BYTE ta_buffer
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
