{-# language OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# language StrictData #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}
{-# language TupleSections #-}

import Control.Lens
import Control.Exception
import Control.Monad.ST (RealWorld)
import Control.Monad
import Data.Atlas
import Data.Bifunctor
import Data.Default
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (partition, scanl')
import Data.Text.Foreign (lengthWord16)
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.Primitive.StateVar
import Data.Proxy
import Data.Set.Lens
import Data.Traversable
import Data.Vector.Generic.Lens
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Ptr.Diff
import GHC.Conc (setUncaughtExceptionHandler)
import qualified GHC.Exts as E
import Graphics.FreeType as FT
import Graphics.GL.Compatibility32
import Graphics.Glow
import Graphics.Harfbuzz hiding (Map)
import Graphics.Harfbuzz.FreeType
import Linear
import qualified SDL
import System.Exit
import System.IO
import System.Random

data TextureAtlas k = TextureAtlas
  { ta_texture :: Texture
  , ta_atlas   :: Atlas RealWorld
  , ta_buffer  :: Ptr ()
  , ta_width   :: Int
  , ta_height  :: Int
  , ta_glyphs  :: IORef (Map k TextureGlyph)
  }

newTextureAtlas :: Int -> Int -> IO (TextureAtlas k)
newTextureAtlas w h = do
  tex <- gen
  withBoundTexture Texture2D tex $ do
    texParameteri Texture2D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri Texture2D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
    glPixelStorei GL_PACK_ALIGNMENT 1
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    atlas_data <- mallocBytes (w * h) -- used as a scratch buffer for texture uploads  w/ glTexSubImage2D
    glTexImage2D GL_TEXTURE_2D 0 GL_LUMINANCE (fromIntegral w) (fromIntegral h) 0 GL_LUMINANCE GL_UNSIGNED_BYTE atlas_data
    atlas <- atlas_create w h
    TextureAtlas tex atlas atlas_data w h <$> newIORef Map.empty

data TextureGlyph = TextureGlyph
  { tg_w, tg_h, tg_x, tg_y, tg_bx, tg_by :: Int
  } deriving (Eq,Show)

-- | Assumes GL_TEXTURE_2D is bound to the atlas texture and that scratch is large enough to handle the bitmap glyph
uploadGlyph :: BitmapGlyph -> Pt -> Ptr () -> IO TextureGlyph
uploadGlyph bg (Pt x y) scratch = do
  withForeignPtr bg $ \p -> do
    Bitmap{..} <- peekDiffOff p bitmapglyph_bitmap_
    let w = fromIntegral bitmap_width
        h = fromIntegral bitmap_rows
        pitch = fromIntegral bitmap_pitch
    target <- if w == pitch
      then pure $ castPtr bitmap_buffer -- usable directly
      else scratch <$ do
        F.for_ [0..h-1] $ \ y -> copyBytes (plusPtr scratch (y*w)) (plusPtr bitmap_buffer (y*pitch)) w -- messy, pack into scratch
    glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE target
    bx <- fromIntegral <$> peekDiffOff p bitmapglyph_left_
    by <- fromIntegral <$> peekDiffOff p bitmapglyph_top_
    pure $ TextureGlyph w h x y bx by
  
-- | render a single glyph
render :: (FT.Face, Codepoint) -> IO BitmapGlyph
render (face,codepoint) = do
  load_glyph face codepoint LOAD_RENDER
  glyphslot <- face_glyph face
  glyph <- get_glyph glyphslot
  glyph_to_bitmap glyph RENDER_MODE_NORMAL def False

size :: BitmapGlyph -> IO Pt
size bmg = withForeignPtr (act bitmapglyph_bitmap_ bmg) $ \bp ->
    Pt <$> do fromIntegral <$> peekDiffOff bp bitmap_width_
       <*> do fromIntegral <$> peekDiffOff bp bitmap_rows_

splat :: [(Maybe a,b)] -> ([(a,b)],[b]) 
splat = bimap (fmap (first fromJust)) (fmap snd) . partition (isJust.fst)

data GlyphTooLarge = GlyphTooLarge deriving (Show,Exception)

-- manages a texture atlas as a cache, grabbing as many items to draw as it can at a time
-- harfbuzz agnostic, uses freetype
batch
  :: (Ord k, Num n)
  => TextureAtlas k -- the atlas to use
  -> [a] -- the list of stuff to draw
  -> V2 n -- a starting position
  -> (a -> V2 n) -- compute an eadvance per item
  -> (k -> (FT.Face,Codepoint)) -- face and codepoint for freetype from keys
  -> (a -> k) -- key extraction
  -> ([(V2 n,a,TextureGlyph)] -> IO ()) -- callback, fed accumulated advances, user values, and valid atlas positions.
  -> IO ()
batch TextureAtlas{..} as0 start advance keyinfo key callback = do
    known <- readIORef ta_glyphs
    let distinct = F.toList $ setOf (folded.to key.filtered (\k -> hasn't (ix k) known)) as0
    fresh <- for distinct $ \k -> (k,) <$> render (keyinfo k)
    go False known fresh $ scanl' (\p a -> p + advance a) start as0 `zip` as0 -- presum the advances
  where
    process known xs as = do
      uploaded <- for xs $ \(xy,(k,bmg)) -> (k,) <$> uploadGlyph bmg xy ta_buffer
      let all_glyphs = known <> Map.fromList uploaded
      writeIORef ta_glyphs all_glyphs
      callback $ as <&> \(adv, a) -> (adv, a, all_glyphs ^?! ix (key a))

    go looped known fresh as = atlas_packM ta_atlas (size.snd) (,) (,) fresh >>= \case
      Right xs -> process known xs as
      Left (splat -> (successes, failures)) -> do
        let failureSet = setOf (folded._1) failures
            (bs, cs) = partition (\a -> key (snd a) `elem` failureSet) as
        process known successes cs
        if null successes && looped
        then throwIO GlyphTooLarge
        else do
          atlas_reset ta_atlas
          writeIORef ta_glyphs Map.empty
          go True Map.empty failures bs

fixed26 :: Integral a => a -> Float
fixed26 x = fromIntegral x / 64

gp_advance :: GlyphPosition -> V2 Float
gp_advance gp = fixed26 <$> V2 (glyph_position_x_advance gp) (glyph_position_y_advance gp)
           
-- A fairly minimal old-school fixed function pipeline demo
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
  ta@TextureAtlas{..} <- newTextureAtlas 140 140

  -- face <- new_face library "test/fonts/Sanskrit2003.ttf" 0
  face <- new_face library "test/fonts/SourceCodePro-Regular.otf" 0
  set_pixel_sizes face 0 128
  font <- hb_ft_font_create face
  buffer <- buffer_create

  --buffer_direction buffer $= DIRECTION_LTR
  --buffer_language buffer $= "hi"
  --buffer_script buffer $= SCRIPT_DEVANAGARI
  --let text = "हालाँकि प्रचलित रूप पूजा"

  buffer_direction buffer $= DIRECTION_LTR
  let text = "this is a test"
  buffer_add_text buffer text 0 (lengthWord16 text)
  shape font buffer mempty

  forever $ do
    events <- SDL.pollEvents
    V2 w h <- SDL.glGetDrawableSize window
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    glOrtho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
    glMatrixMode GL_MODELVIEW
    glLoadIdentity
    glDisable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    glClear GL_COLOR_BUFFER_BIT
    let yb = fromIntegral h / 2

    withBoundTexture Texture2D ta_texture $ do
      glEnable GL_TEXTURE_2D
      gs <- zip
        <$> do E.toList <$> buffer_get_glyph_infos buffer 
        <*> do E.toList <$> buffer_get_glyph_positions buffer
      batch ta gs (V2 100 yb) (gp_advance.snd) ((,) face) (\(gi,gp) -> glyph_info_codepoint gi) $ \ gs -> do
        glBegin GL_QUADS
        for gs $ \(V2 x y,(_,gp),TextureGlyph{..}) -> do
          let tx x = fromIntegral x / fromIntegral ta_width
              ty y = fromIntegral y / fromIntegral ta_height
              s0 = tx tg_x; s1 = s0 + tx tg_w
              t0 = ty tg_y; t1 = t0 + ty tg_h
              x0 = x + fixed26 (glyph_position_x_offset gp) + fromIntegral tg_bx
              y0 = fromIntegral $ floor $ y + fixed26 (glyph_position_y_offset gp) + fromIntegral tg_by
              x1 = x0 + fromIntegral tg_w
              y1 = fromIntegral $ floor $ y0 - fromIntegral tg_h
          glTexCoord2f s0 t0
          glVertex2f x0 y0
          glTexCoord2f s0 t1
          glVertex2f x0 y1
          glTexCoord2f s1 t1
          glVertex2f x1 y1
          glTexCoord2f s1 t0
          glVertex2f x1 y0
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
