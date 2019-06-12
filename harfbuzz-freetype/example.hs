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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Coerce
import Data.Default
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Ap(..))
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
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Ptr.Diff
import GHC.Conc (setUncaughtExceptionHandler)
import qualified GHC.Exts as E
import Graphics.FreeType as FT
import Graphics.GL.Core41
import Graphics.Glow
import Graphics.Harfbuzz hiding (Map)
import Graphics.Harfbuzz.FreeType
import Linear
import Numeric.Fixed.F26Dot6
import qualified SDL
import System.Exit
import System.IO
import System.Random

data TextureAtlas k = TextureAtlas
  { ta_texture :: Texture
  , ta_atlas   :: Atlas RealWorld
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
    glTexImage2D GL_TEXTURE_2D 0 GL_R8 (fromIntegral w) (fromIntegral h) 0 GL_RED GL_UNSIGNED_BYTE nullPtr
    -- texParameter4i Texture2D GL_TEXTURE_SWIZZLE_RGBA $= V4 GL_ONE GL_ONE GL_ONE GL_RED
    atlas <- atlas_create w h
    TextureAtlas tex atlas w h <$> newIORef Map.empty

resetTextureAtlas :: TextureAtlas k -> IO ()
resetTextureAtlas TextureAtlas{..} = do
  atlas_reset ta_atlas
  glTexImage2D GL_TEXTURE_2D 0 GL_R8 (fromIntegral ta_width) (fromIntegral ta_height) 0 GL_RED GL_UNSIGNED_BYTE nullPtr
  writeIORef ta_glyphs Map.empty

data TextureGlyph = TextureGlyph
  { tg_w, tg_h, tg_x, tg_y, tg_bx, tg_by :: Int
  } deriving (Eq,Show)

data BadBitmapFormat = BadBitmapFormat deriving (Show,Exception)

-- | Assumes GL_TEXTURE_2D is bound to the atlas texture and that scratch is large enough to handle the bitmap glyph
uploadGlyph :: BitmapGlyph -> Pt -> IO TextureGlyph
uploadGlyph bg (Pt x y) = do
  withForeignPtr bg $ \p -> do
    Bitmap{..} <- peekDiffOff p bitmapglyph_bitmap_
    let w = fromIntegral bitmap_width
        h = fromIntegral bitmap_rows
        pitch = fromIntegral bitmap_pitch
        draw = glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE
    unless (w*h == 0) $
      if w /= pitch
      then allocaBytes (w*h) $ \ scratch -> do
        F.for_ [0..h-1] $ \ y -> copyBytes (plusPtr scratch (y*w)) (plusPtr bitmap_buffer (y*pitch)) w
        draw scratch
      else draw $ castPtr bitmap_buffer
    TextureGlyph w h x y
      <$> do fromIntegral <$> peekDiffOff p bitmapglyph_left_
      <*> do fromIntegral <$> peekDiffOff p bitmapglyph_top_

-- | render a single glyph
render :: (FT.Face, Codepoint) -> IO BitmapGlyph
render (face,codepoint) = do
  load_glyph face codepoint LOAD_RENDER
  glyph <- get_glyph $ face_glyph face
  glyph_to_bitmap glyph RENDER_MODE_NORMAL def False

translate26dot6 :: V2 F26Dot6 -> BBox -> BBox
translate26dot6 (V2 x y) (BBox a b c d) = BBox
  (floor $ x + fromIntegral a) (floor $ y + fromIntegral b)
  (ceiling $ x + fromIntegral c) (ceiling $ y + fromIntegral d)

tg_box :: TextureGlyph -> BBox
tg_box TextureGlyph{..} = BBox (fromIntegral tg_bx) (fromIntegral $ tg_by - tg_h) (fromIntegral $ tg_bx + tg_w) (fromIntegral $ tg_by )

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
  :: Ord k
  => TextureAtlas k -- the atlas to use
  -> [a] -- the list of stuff to draw
  -> (a -> V2 F26Dot6) -- compute an advance per item, from harfbuzz, ignoring freetype
  -> (k -> (FT.Face,Codepoint)) -- face and codepoint for freetype from keys
  -> (a -> k) -- key extraction
  -> (BBox -> [(V2 F26Dot6,a,TextureGlyph)] -> IO ()) -- callback, fed accumulated advances, user values, and valid atlas positions.
  -> IO ()
batch ta@TextureAtlas{..} as0 advance keyinfo key callback = do
    known <- readIORef ta_glyphs
    let distinct = F.toList $ setOf (folded.to key.filtered (\k -> hasn't (ix k) known)) as0
        aas0 = zip (scanl' (\p a -> p + advance a) (V2 0 0) as0) as0
    fresh <- for distinct $ \k -> (k,) <$> render (keyinfo k)
    let freshMap = Map.fromList fresh
    box <- getAp $ flip foldMap aas0 $ \ (adv, key -> k) -> Ap $ translate26dot6 adv <$> case known^.at k of
      Just tg -> pure (tg_box tg)
      Nothing -> glyph_get_cbox (freshMap^?!ix k) GLYPH_BBOX_PIXELS
    go False known fresh aas0 box
  where
    process known xs as box = do
      uploaded <- for xs $ \(xy,(k,bmg)) -> (k,) <$> uploadGlyph bmg xy
      let all_glyphs = known <> Map.fromList uploaded
      writeIORef ta_glyphs all_glyphs
      callback box $ as <&> \(adv, a) -> (adv, a, all_glyphs ^?! ix (key a))

    go looped known fresh as box = atlas_packM ta_atlas (size.snd) (,) (,) fresh >>= \case
      Right xs -> process known xs as box
      Left (splat -> (successes, failures)) -> do
        let failureSet = setOf (folded._1) failures
            (bs, cs) = partition (\a -> key (snd a) `elem` failureSet) as
        process known successes cs box
        when (null successes && looped) $ throwIO GlyphTooLarge -- grow atlas texture in this event?
        resetTextureAtlas ta
        go True Map.empty failures bs box

fixed26 :: Integral a => a -> Float
fixed26 x = fromIntegral x / 64

gp_advance :: GlyphPosition -> V2 F26Dot6
gp_advance gp = F26Dot6 . fromIntegral <$> V2 (glyph_position_x_advance gp) (glyph_position_y_advance gp)

-- A fairly minimal old-school fixed function pipeline demo
main :: IO ()
main = do
  setUncaughtExceptionHandler $ putStrLn . displayException
  hSetBuffering stdout NoBuffering
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "harfbuzz-freetype example" SDL.defaultWindow
    { SDL.windowInitialSize = V2 640 200
    , SDL.windowHighDPI = True
    , SDL.windowOpenGL = Just SDL.defaultOpenGL
      { SDL.glProfile = SDL.Core SDL.Debug 4 1
      }
    }
  SDL.showWindow window
  _ <- SDL.glCreateContext window

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  vs <- BS.readFile "shaders/example.vert" >>= buildShaderFrom VertexShader
  fs <- BS.readFile "shaders/example.frag" >>= buildShaderFrom FragmentShader
  program <- Program <$> glCreateProgram
  withCString "a" $ glBindAttribLocation (coerce program) 0
  withCString "b" $ glBindAttribLocation (coerce program) 1
  attachShader program vs
  attachShader program fs
  glLinkProgram (coerce program)
  linkStatus program >>= \ok -> unless ok $ do
    programInfoLog program >>= B8.putStrLn
    exitFailure
  currentProgram $= program
  uniformLocation program "tex" >>= \texul -> programUniform1i program texul $= 0 -- bind to GL_TEXTURE_2D
  vao <- gen
  boundVertexArray $= vao
  vbo <- gen
  boundBufferAt ArrayBufferTarget $= vbo
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 4 GL_FLOAT GL_FALSE 32 nullPtr
  glVertexAttribDivisor 0 1
  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 4 GL_FLOAT GL_FALSE 32 (intPtrToPtr 16)
  glVertexAttribDivisor 1 1

  ta@TextureAtlas{..} <- newTextureAtlas 1024 1024
  boundTexture Texture2D $= ta_texture
  let tx x = fromIntegral x / fromIntegral ta_width
      ty y = fromIntegral y / fromIntegral ta_height

  library <- init_library
  face <- new_face library "test/fonts/SourceCodePro-Regular.otf" 0
  set_pixel_sizes face 0 32

  font <- hb_ft_font_create face
  buffer <- buffer_create
  buffer_direction buffer $= DIRECTION_LTR
  let text = "This is a somewhat long test. I would like to see more."
  buffer_add_text buffer text 0 (lengthWord16 text)
  shape font buffer mempty
  gs <- zip
    <$> do E.toList <$> buffer_get_glyph_infos buffer
    <*> do E.toList <$> buffer_get_glyph_positions buffer

  forever $ do
    events <- SDL.pollEvents
    V2 w h <- SDL.glGetDrawableSize window
    let cx = fromIntegral w / 2
        cy = fromIntegral h / 2
        sx x = x * recip cx - 1
        sy y = y * recip cy - 1
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glClear GL_COLOR_BUFFER_BIT
    batch ta gs (gp_advance.snd) ((,) face) (\(gi,gp) -> glyph_info_codepoint gi) $ \ (BBox llx lly urx ury) tgs -> do
      let -- bw = fromIntegral $ urx - llx -- bbox width
          -- bh = fromIntegral $ ury - lly -- bbox height
          bx = fromIntegral llx -- left justify, ignoring baseline, etc.
          by = fromIntegral h - fromIntegral ury -- top vertical justification
          content = V.fromList $ do
            (V2 x y,(_,gp),TextureGlyph{..}) <- tgs
            let x0 = bx + realToFrac x + fixed26 (glyph_position_x_offset gp) + fromIntegral tg_bx
                y0 = fromIntegral $ floor (by + realToFrac y + fixed26 (glyph_position_y_offset gp) + fromIntegral tg_by)
                x1 = x0 + fromIntegral tg_w
                y1 = fromIntegral $ floor (y0 - fromIntegral tg_h)
            [sx x0,tx tg_x,sx x1,tx (tg_x + tg_w),sy y0,ty tg_y,sy y1,ty (tg_y + tg_h) :: Float] -- 8 floats per glyph
      bufferData ArrayBufferTarget $= (StreamDraw, content)
      glDrawArraysInstanced GL_TRIANGLE_STRIP 4 GL_UNSIGNED_INT $ fromIntegral $ div (V.length content) 8
    SDL.glSwapWindow window

    when (any escOrQuit events) $ do
      putStrLn "Cleaning up"
      boundTexture Texture2D $= def
      boundBufferAt ElementArrayBufferTarget $= def -- IBO
      boundBufferAt ArrayBufferTarget $= def -- VBO
      boundVertexArray $= def -- VAO
      glDisableVertexAttribArray 0
      glDisableVertexAttribArray 1
      currentProgram $= def
      SDL.destroyWindow window
      SDL.quit
      exitSuccess

escOrQuit :: SDL.Event -> Bool
escOrQuit (SDL.Event _ e) = case e of
  SDL.QuitEvent -> True
  SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ SDL.KeycodeEscape _)) -> True
  _ -> False
