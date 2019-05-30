{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
module Graphics.Harfbuzz.Font
( Font
, font_create
, font_create_sub_font
, font_face -- statevar
, font_get_extents_for_direction
, font_get_glyph
, font_get_glyph_advance_for_direction
, font_get_glyph_advances_for_direction
, font_get_glyph_contour_point
, font_get_glyph_contour_point_for_origin
, font_get_glyph_extents
, font_get_glyph_extents_for_origin
, font_get_glyph_name
, font_get_glyph_from_name
, font_ppem -- statevar
, font_ptem -- statevar
, font_scale -- statevar
, font_set_funcs
, font_glyph_to_string
, font_glyph_from_string
, font_get_glyph_origin_for_direction
, font_add_glyph_origin_for_direction
, font_subtract_glyph_origin_for_direction
, font_set_variations
, font_set_var_coords_design
, font_var_coords_normalized -- statevar
, FontExtents -- no constructor, partial fields
  ( font_extents_ascender
  , font_extents_descender
  , font_extents_line_gap
  )
, FontFuncs
, font_funcs_create
, font_funcs_is_immutable
, font_funcs_make_immutable
--  , font_funcs_set_glyph_contour_point_func
--  , font_funcs_set_glyph_extents_func
--  , font_funcs_set_glyph_from_name_func
--  , font_funcs_set_glyph_h_advance_func
--  , font_funcs_set_glyph_h_advances_func
--  , font_funcs_set_glyph_h_origin_func
--  , font_funcs_set_glyph_name_func
--  , font_funcs_set_glyph_v_advance_func
--  , font_funcs_set_glyph_v_advances_func
--  , font_funcs_set_glyph_v_origin_func
--  , font_funcs_set_glyph_nominal_glyph_func
--  , font_funcs_set_variation_glyph_func
) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Functor ((<&>))
import Data.StateVar
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

font_create :: MonadIO m => Face -> m Font
font_create face = liftIO $ [C.exp|hb_font_t * { hb_font_create($face:face) }|] >>= foreignFont

font_create_sub_font :: MonadIO m => Font -> m Font
font_create_sub_font parent = liftIO $ [C.exp|hb_font_t * {
    hb_font_create_sub_font(hb_font_reference($font:parent))
  }|] >>= foreignFont

-- font_get_face :: MonadIO m => Font -> m Face
-- font_get_face font = liftIO $ [C.exp|hb_face_t * { hb_face_reference(hb_font_get_face($font:font)) }|] >>= foreignFace

font_face :: Font -> StateVar Face
font_face font = StateVar g s where
  g = [C.exp|hb_face_t * { hb_face_reference(hb_font_get_face($font:font)) }|] >>= foreignFace
  s face = [C.block|void { hb_font_set_face($font:font,hb_face_reference($face:face)); }|]

font_get_glyph :: MonadIO m => Font -> Codepoint -> Codepoint -> m (Maybe Codepoint)
font_get_glyph font unicode variation_selector = liftIO $ alloca $ \pglyph -> do
  b <- [C.exp|hb_bool_t {
    hb_font_get_glyph($font:font,$(hb_codepoint_t unicode),$(hb_codepoint_t variation_selector),$(hb_codepoint_t * pglyph))
  }|]
  if cbool b then Just <$> peek pglyph else pure Nothing

font_get_glyph_advance_for_direction :: MonadIO m => Font -> Codepoint -> Direction -> m (Position, Position)
font_get_glyph_advance_for_direction font glyph dir = liftIO $ allocaArray 2 $ \xy -> do
  [C.block|void {
    hb_position_t * xy = $(hb_position_t * xy);
    hb_font_get_glyph_advance_for_direction($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),xy,xy+1);
  }|]
  (,) <$> peek xy <*> peek (advancePtr xy 1)

-- You'll need to manage the glyphs and advances yourself
font_get_glyph_advances_for_direction :: MonadIO m => Font -> Direction -> Int -> Ptr Codepoint -> Int -> Ptr Position -> Int -> m ()
font_get_glyph_advances_for_direction
  font dir (fromIntegral -> count) first_glyph (fromIntegral -> glyph_stride) first_advance (fromIntegral -> advance_stride) = liftIO
  [C.block|void {
    hb_font_get_glyph_advances_for_direction(
      $font:font,
      $(hb_direction_t dir),
      $(unsigned int count),
      $(const hb_codepoint_t * first_glyph),
      $(unsigned int glyph_stride),
      $(hb_position_t * first_advance),
      $(unsigned int advance_stride)
    );
  }|]

font_get_glyph_contour_point :: MonadIO m => Font -> Codepoint -> Int -> m (Maybe (Position, Position))
font_get_glyph_contour_point font glyph (fromIntegral -> point_index) = liftIO $ allocaArray 2 $ \xy -> do
  b <- [C.block|hb_bool_t {
    hb_position_t * xy = $(hb_position_t * xy);
    return hb_font_get_glyph_contour_point($font:font,$(hb_codepoint_t glyph),$(unsigned int point_index),xy,xy+1);
  }|]
  if cbool b
  then do
    x <- peek xy
    y <- peek (advancePtr xy 1)
    pure $ Just (x,y)
  else pure Nothing

font_get_glyph_contour_point_for_origin :: MonadIO m => Font -> Codepoint -> Int -> Direction -> m (Maybe (Position, Position))
font_get_glyph_contour_point_for_origin font glyph (fromIntegral -> point_index) dir = liftIO $ allocaArray 2 $ \xy -> do
  b <- [C.block|hb_bool_t {
    hb_position_t * xy = $(hb_position_t * xy);
    return hb_font_get_glyph_contour_point_for_origin($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),$(unsigned int point_index),xy,xy+1);
  }|]
  if cbool b
  then do
    x <- peek xy
    y <- peek (advancePtr xy 1)
    pure $ Just (x,y)
  else pure Nothing

font_get_glyph_origin_for_direction :: MonadIO m => Font -> Codepoint -> Direction -> m (Position, Position)
font_get_glyph_origin_for_direction font glyph dir = liftIO $ allocaArray 2 $ \xy -> do
  [C.block|void {
    hb_position_t * xy = $(hb_position_t * xy);
    hb_font_get_glyph_origin_for_direction($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),xy,xy+1);
  }|]
  (,) <$> peek xy <*> peek (advancePtr xy 1)

font_add_glyph_origin_for_direction :: MonadIO m => Font -> Codepoint -> Direction -> (Position,Position) -> m (Position,Position)
font_add_glyph_origin_for_direction font glyph dir (x,y) = do
  (dx,dy) <- font_get_glyph_origin_for_direction font glyph dir
  pure (x + dx, y + dy)

font_subtract_glyph_origin_for_direction :: MonadIO m => Font -> Codepoint -> Direction -> (Position,Position) -> m (Position,Position)
font_subtract_glyph_origin_for_direction font glyph dir (x,y) = do
  (dx,dy) <- font_get_glyph_origin_for_direction font glyph dir
  pure (x - dx, y - dy)

font_set_variations :: MonadIO m => Font -> [Variation] -> m ()
font_set_variations font vars = liftIO $ withArrayLen vars $ \ (fromIntegral -> len) pvars ->
   [C.block|void{ hb_font_set_variations($font:font,$(const hb_variation_t * pvars),$(unsigned int len)); }|]

font_set_var_coords_design :: MonadIO m => Font -> [Float] -> m ()
font_set_var_coords_design font v = liftIO $ withArrayLen (coerce <$> v) $ \ (fromIntegral -> len) pcoords ->
  [C.block|void{ hb_font_set_var_coords_design($font:font,$(const float * pcoords),$(unsigned int len)); }|]

font_var_coords_normalized :: Font -> StateVar [Int]
font_var_coords_normalized font = StateVar g s where
  g :: IO [Int]
  g = alloca $ \plen -> do
    result <- [C.exp|const int * { hb_font_get_var_coords_normalized($font:font,$(unsigned int * plen)) }|]
    len <- peek plen
    fmap fromIntegral <$> peekArray (fromIntegral len) result
  s v = withArrayLen (fromIntegral <$> v) $ \ (fromIntegral -> len) pcoords ->
    [C.block|void{ hb_font_set_var_coords_normalized($font:font,$(const int * pcoords),$(unsigned int len)); }|]

font_get_glyph_extents :: MonadIO m => Font -> Codepoint -> m (Maybe GlyphExtents)
font_get_glyph_extents font glyph = liftIO $ alloca $ \extents -> do
  b <- [C.exp|hb_bool_t { hb_font_get_glyph_extents($font:font,$(hb_codepoint_t glyph),$(hb_glyph_extents_t * extents)) }|]
  if cbool b then Just <$> peek extents else pure Nothing

font_get_glyph_extents_for_origin :: MonadIO m => Font -> Codepoint -> Direction -> m (Maybe GlyphExtents)
font_get_glyph_extents_for_origin font glyph dir = liftIO $ alloca $ \extents -> do
  b <- [C.exp|hb_bool_t { hb_font_get_glyph_extents_for_origin($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),$(hb_glyph_extents_t * extents)) }|]
  if cbool b then Just <$> peek extents else pure Nothing

font_get_extents_for_direction :: MonadIO m => Font -> Direction -> m FontExtents
font_get_extents_for_direction font dir = liftIO $ alloca $ \ extents ->
  [C.block|void {
    hb_font_get_extents_for_direction($font:font,$(hb_direction_t dir),$(hb_font_extents_t * extents));
  }|] *> peek extents

font_get_glyph_name :: MonadIO m => Font -> Codepoint -> m (Maybe String)
font_get_glyph_name font glyph = liftIO $ allocaBytes 4096 $ \buf -> do
  b <- [C.exp|hb_bool_t { hb_font_get_glyph_name($font:font,$(hb_codepoint_t glyph),$(char * buf),4095) }|]
  if cbool b then Just <$> peekCString buf else pure Nothing

font_get_glyph_from_name :: MonadIO m => Font -> String -> m (Maybe Codepoint)
font_get_glyph_from_name font name = liftIO $
  alloca $ \glyph ->
    withCStringLen name $ \ (cstr,fromIntegral -> len) -> do
      b <- [C.exp|hb_bool_t {
        hb_font_get_glyph_from_name($font:font,$(const char * cstr),$(unsigned int len), $(hb_codepoint_t * glyph))
      }|]
      if cbool b then Just <$> peek glyph else pure Nothing

font_glyph_to_string :: MonadIO m => Font -> Codepoint -> m String
font_glyph_to_string font glyph = liftIO $ allocaBytes 4096 $ \buf -> do
  [C.block|void { hb_font_glyph_to_string($font:font,$(hb_codepoint_t glyph),$(char * buf),4095); }|]
  peekCString buf

font_glyph_from_string :: MonadIO m => Font -> String -> m (Maybe Codepoint)
font_glyph_from_string font name = liftIO $
  alloca $ \glyph ->
    withCStringLen name $ \ (cstr,fromIntegral -> len) -> do
      b <- [C.exp|hb_bool_t {
        hb_font_glyph_from_string($font:font,$(const char * cstr),$(unsigned int len), $(hb_codepoint_t * glyph))
      }|]
      if cbool b then Just <$> peek glyph else pure Nothing

font_ppem :: Font -> StateVar (Int,Int)
font_ppem font = StateVar g s where
  g = allocaArray 2 $ \xy -> do
    [C.block|void {
       unsigned int * xy = $(unsigned int * xy);
       hb_font_get_ppem($font:font,xy,xy+1);
    }|]
    a <- peek xy
    b <- peek (advancePtr xy 1)
    return (fromIntegral a,fromIntegral b)
  s (fromIntegral -> x, fromIntegral -> y) = [C.block|void { hb_font_set_ppem($font:font,$(unsigned int x),$(unsigned int y)); }|]

font_ptem :: Font -> StateVar Float
font_ptem font = StateVar g s where
  g = [C.exp|float { hb_font_get_ptem($font:font) }|] <&> coerce
  s (coerce -> x) = [C.block|void { hb_font_set_ptem($font:font,$(float x)); }|]

font_scale :: Font -> StateVar (Int,Int)
font_scale font = StateVar g s where
  g = allocaArray 2 $ \xy -> do
    [C.block|void {
       int * xy = $(int * xy);
       hb_font_get_scale($font:font,xy,xy+1);
    }|]
    a <- peek xy
    b <- peek (advancePtr xy 1)
    return (fromIntegral a,fromIntegral b)
  s (fromIntegral -> x, fromIntegral -> y) = [C.block|void { hb_font_set_scale($font:font,$(int x),$(int y)); }|]

font_set_funcs :: MonadIO m => Font -> FontFuncs -> Ptr () -> FinalizerPtr () -> m ()
font_set_funcs font funcs font_data destroy = liftIO [C.block|void {
  hb_font_set_funcs($font:font,hb_font_funcs_reference($font-funcs:funcs),$(void * font_data),$(hb_destroy_func_t destroy));
}|]

font_funcs_create :: MonadIO m => m FontFuncs
font_funcs_create = liftIO $ [C.exp|hb_font_funcs_t * { hb_font_funcs_create() }|] >>= foreignFontFuncs

font_funcs_is_immutable :: MonadIO m => FontFuncs -> m Bool
font_funcs_is_immutable b = liftIO $ [C.exp|hb_bool_t { hb_font_funcs_is_immutable($font-funcs:b) }|] <&> cbool

font_funcs_make_immutable :: MonadIO m => FontFuncs -> m ()
font_funcs_make_immutable b = liftIO [C.block|void { hb_font_funcs_make_immutable($font-funcs:b); }|]
