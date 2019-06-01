{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
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

import Control.Monad.Primitive
import Data.Coerce
import Data.Functor ((<&>))
import Data.Primitive.StateVar
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Private
import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

font_create :: PrimMonad m => Face (PrimState m) -> m (Font (PrimState m))
font_create face = unsafeIOToPrim $ [C.exp|hb_font_t * { hb_font_create($face:face) }|] >>= foreignFont

font_create_sub_font :: PrimMonad m => Font (PrimState m) -> m (Font (PrimState m))
font_create_sub_font parent = unsafeIOToPrim $ [C.exp|hb_font_t * {
    hb_font_create_sub_font(hb_font_reference($font:parent))
  }|] >>= foreignFont

-- font_get_face :: PrimMonad m => Font -> m Face
-- font_get_face font = unsafeIOToPrim $ [C.exp|hb_face_t * { hb_face_reference(hb_font_get_face($font:font)) }|] >>= foreignFace

font_face :: Font s -> StateVar s (Face s)
font_face font = unsafeStateVar g s where
  g = [C.exp|hb_face_t * { hb_face_reference(hb_font_get_face($font:font)) }|] >>= foreignFace
  s face = [C.block|void { hb_font_set_face($font:font,hb_face_reference($face:face)); }|]

font_get_glyph :: PrimMonad m => Font (PrimState m) -> Codepoint -> Codepoint -> m (Maybe Codepoint)
font_get_glyph font unicode variation_selector = unsafeIOToPrim $ alloca $ \pglyph -> do
  b <- [C.exp|hb_bool_t {
    hb_font_get_glyph($font:font,$(hb_codepoint_t unicode),$(hb_codepoint_t variation_selector),$(hb_codepoint_t * pglyph))
  }|]
  if cbool b then Just <$> peek pglyph else pure Nothing

font_get_glyph_advance_for_direction :: PrimMonad m => Font (PrimState m) -> Codepoint -> Direction -> m (Position, Position)
font_get_glyph_advance_for_direction font glyph dir = unsafeIOToPrim $ allocaArray 2 $ \xy -> do
  [C.block|void {
    hb_position_t * xy = $(hb_position_t * xy);
    hb_font_get_glyph_advance_for_direction($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),xy,xy+1);
  }|]
  (,) <$> peek xy <*> peek (advancePtr xy 1)

-- You'll need to manage the glyphs and advances yourself
font_get_glyph_advances_for_direction :: PrimMonad m => Font (PrimState m) -> Direction -> Int -> Ptr Codepoint -> Int -> Ptr Position -> Int -> m ()
font_get_glyph_advances_for_direction
  font dir (fromIntegral -> count) first_glyph (fromIntegral -> glyph_stride) first_advance (fromIntegral -> advance_stride) = unsafeIOToPrim
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

font_get_glyph_contour_point :: PrimMonad m => Font (PrimState m) -> Codepoint -> Int -> m (Maybe (Position, Position))
font_get_glyph_contour_point font glyph (fromIntegral -> point_index) = unsafeIOToPrim $ allocaArray 2 $ \xy -> do
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

font_get_glyph_contour_point_for_origin :: PrimMonad m => Font (PrimState m) -> Codepoint -> Int -> Direction -> m (Maybe (Position, Position))
font_get_glyph_contour_point_for_origin font glyph (fromIntegral -> point_index) dir = unsafeIOToPrim $ allocaArray 2 $ \xy -> do
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

font_get_glyph_origin_for_direction :: PrimMonad m => Font (PrimState m) -> Codepoint -> Direction -> m (Position, Position)
font_get_glyph_origin_for_direction font glyph dir = unsafeIOToPrim $ allocaArray 2 $ \xy -> do
  [C.block|void {
    hb_position_t * xy = $(hb_position_t * xy);
    hb_font_get_glyph_origin_for_direction($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),xy,xy+1);
  }|]
  (,) <$> peek xy <*> peek (advancePtr xy 1)

font_add_glyph_origin_for_direction :: PrimMonad m => Font (PrimState m) -> Codepoint -> Direction -> (Position,Position) -> m (Position,Position)
font_add_glyph_origin_for_direction font glyph dir (x,y) = do
  (dx,dy) <- font_get_glyph_origin_for_direction font glyph dir
  pure (x + dx, y + dy)

font_subtract_glyph_origin_for_direction :: PrimMonad m => Font (PrimState m) -> Codepoint -> Direction -> (Position,Position) -> m (Position,Position)
font_subtract_glyph_origin_for_direction font glyph dir (x,y) = do
  (dx,dy) <- font_get_glyph_origin_for_direction font glyph dir
  pure (x - dx, y - dy)

font_set_variations :: PrimMonad m => Font (PrimState m) -> [Variation] -> m ()
font_set_variations font vars = unsafeIOToPrim $ withArrayLen vars $ \ (fromIntegral -> len) pvars ->
   [C.block|void{ hb_font_set_variations($font:font,$(const hb_variation_t * pvars),$(unsigned int len)); }|]

font_set_var_coords_design :: PrimMonad m => Font (PrimState m) -> [Float] -> m ()
font_set_var_coords_design font v = unsafeIOToPrim $ withArrayLen (coerce <$> v) $ \ (fromIntegral -> len) pcoords ->
  [C.block|void{ hb_font_set_var_coords_design($font:font,$(const float * pcoords),$(unsigned int len)); }|]

font_var_coords_normalized :: Font s -> StateVar s [Int]
font_var_coords_normalized font = unsafeStateVar g s where
  g = alloca $ \plen -> do
    result <- [C.exp|const int * { hb_font_get_var_coords_normalized($font:font,$(unsigned int * plen)) }|]
    len <- peek plen
    fmap fromIntegral <$> peekArray (fromIntegral len) result
  s v = withArrayLen (fromIntegral <$> v) $ \ (fromIntegral -> len) pcoords ->
    [C.block|void{ hb_font_set_var_coords_normalized($font:font,$(const int * pcoords),$(unsigned int len)); }|]

font_get_glyph_extents :: PrimMonad m => Font (PrimState m) -> Codepoint -> m (Maybe GlyphExtents)
font_get_glyph_extents font glyph = unsafeIOToPrim $ alloca $ \extents -> do
  b <- [C.exp|hb_bool_t { hb_font_get_glyph_extents($font:font,$(hb_codepoint_t glyph),$(hb_glyph_extents_t * extents)) }|]
  if cbool b then Just <$> peek extents else pure Nothing

font_get_glyph_extents_for_origin :: PrimMonad m => Font (PrimState m) -> Codepoint -> Direction -> m (Maybe GlyphExtents)
font_get_glyph_extents_for_origin font glyph dir = unsafeIOToPrim $ alloca $ \extents -> do
  b <- [C.exp|hb_bool_t { hb_font_get_glyph_extents_for_origin($font:font,$(hb_codepoint_t glyph),$(hb_direction_t dir),$(hb_glyph_extents_t * extents)) }|]
  if cbool b then Just <$> peek extents else pure Nothing

font_get_extents_for_direction :: PrimMonad m => Font (PrimState m) -> Direction -> m FontExtents
font_get_extents_for_direction font dir = unsafeIOToPrim $ alloca $ \ extents ->
  [C.block|void {
    hb_font_get_extents_for_direction($font:font,$(hb_direction_t dir),$(hb_font_extents_t * extents));
  }|] *> peek extents

font_get_glyph_name :: PrimMonad m => Font (PrimState m) -> Codepoint -> m (Maybe String)
font_get_glyph_name font glyph = unsafeIOToPrim $ allocaBytes 4096 $ \buf -> do
  b <- [C.exp|hb_bool_t { hb_font_get_glyph_name($font:font,$(hb_codepoint_t glyph),$(char * buf),4095) }|]
  if cbool b then Just <$> peekCString buf else pure Nothing

font_get_glyph_from_name :: PrimMonad m => Font (PrimState m) -> String -> m (Maybe Codepoint)
font_get_glyph_from_name font name = unsafeIOToPrim $
  alloca $ \glyph ->
    withCStringLen name $ \ (cstr,fromIntegral -> len) -> do
      b <- [C.exp|hb_bool_t {
        hb_font_get_glyph_from_name($font:font,$(const char * cstr),$(unsigned int len), $(hb_codepoint_t * glyph))
      }|]
      if cbool b then Just <$> peek glyph else pure Nothing

font_glyph_to_string :: PrimMonad m => Font (PrimState m) -> Codepoint -> m String
font_glyph_to_string font glyph = unsafeIOToPrim $ allocaBytes 4096 $ \buf -> do
  [C.block|void { hb_font_glyph_to_string($font:font,$(hb_codepoint_t glyph),$(char * buf),4095); }|]
  peekCString buf

font_glyph_from_string :: PrimMonad m => Font (PrimState m) -> String -> m (Maybe Codepoint)
font_glyph_from_string font name = unsafeIOToPrim $
  alloca $ \glyph ->
    withCStringLen name $ \ (cstr,fromIntegral -> len) -> do
      b <- [C.exp|hb_bool_t {
        hb_font_glyph_from_string($font:font,$(const char * cstr),$(unsigned int len), $(hb_codepoint_t * glyph))
      }|]
      if cbool b then Just <$> peek glyph else pure Nothing

font_ppem :: Font s -> StateVar s (Int,Int)
font_ppem font = unsafeStateVar g s where
  g = allocaArray 2 $ \xy -> do
    [C.block|void {
       unsigned int * xy = $(unsigned int * xy);
       hb_font_get_ppem($font:font,xy,xy+1);
    }|]
    a <- peek xy
    b <- peek (advancePtr xy 1)
    return (fromIntegral a,fromIntegral b)
  s (fromIntegral -> x, fromIntegral -> y) = [C.block|void { hb_font_set_ppem($font:font,$(unsigned int x),$(unsigned int y)); }|]

font_ptem :: Font s -> StateVar s Float
font_ptem font = unsafeStateVar g s where
  g = [C.exp|float { hb_font_get_ptem($font:font) }|] <&> coerce
  s (coerce -> x) = [C.block|void { hb_font_set_ptem($font:font,$(float x)); }|]

font_scale :: Font s -> StateVar s (Int,Int)
font_scale font = unsafeStateVar g s where
  g = allocaArray 2 $ \xy -> do
    [C.block|void {
       int * xy = $(int * xy);
       hb_font_get_scale($font:font,xy,xy+1);
    }|]
    a <- peek xy
    b <- peek (advancePtr xy 1)
    return (fromIntegral a,fromIntegral b)
  s (fromIntegral -> x, fromIntegral -> y) = [C.block|void { hb_font_set_scale($font:font,$(int x),$(int y)); }|]

font_set_funcs :: PrimMonad m => Font (PrimState m) -> FontFuncs (PrimState m) -> Ptr () -> FinalizerPtr () -> m ()
font_set_funcs font funcs font_data destroy = unsafeIOToPrim [C.block|void {
  hb_font_set_funcs($font:font,hb_font_funcs_reference($font-funcs:funcs),$(void * font_data),$(hb_destroy_func_t destroy));
}|]

font_funcs_create :: PrimMonad m => m (FontFuncs (PrimState m))
font_funcs_create = unsafeIOToPrim $ [C.exp|hb_font_funcs_t * { hb_font_funcs_create() }|] >>= foreignFontFuncs

font_funcs_is_immutable :: PrimMonad m => FontFuncs (PrimState m) -> m Bool
font_funcs_is_immutable b = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_font_funcs_is_immutable($font-funcs:b) }|] <&> cbool

font_funcs_make_immutable :: PrimMonad m => FontFuncs (PrimState m) -> m ()
font_funcs_make_immutable b = unsafeIOToPrim [C.block|void { hb_font_funcs_make_immutable($font-funcs:b); }|]
