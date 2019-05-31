{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.OpenType.Math
( MathConstant(..)
, MathKern(..)
, MathGlyphPart(..)
, MathGlyphPartFlags(..)
, MathGlyphVariant(..)
, math_has_data
, math_get_constant
, math_get_glyph_italics_correction
, math_get_glyph_kerning
, math_get_glyph_top_accent_attachment
, math_get_min_connector_overlap
, math_is_glyph_extended_shape
, math_get_glyph_variants
, math_get_glyph_assembly
, pattern TAG_MATH
, pattern MATH_SCRIPT
) where

import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal
import Graphics.Harfbuzz.OpenType.Private

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- * Math

math_has_data :: MonadIO m => Face -> m Bool
math_has_data face = liftIO $ [C.exp|hb_bool_t { hb_ot_math_has_data($face:face) }|] <&> cbool

math_get_constant :: MonadIO m => Font -> MathConstant -> m Position
math_get_constant font k = liftIO
  [C.exp|hb_position_t { hb_ot_math_get_constant($font:font,$(hb_ot_math_constant_t k)) }|]

math_get_glyph_italics_correction :: MonadIO m => Font -> Codepoint -> m Position
math_get_glyph_italics_correction font glyph = liftIO
  [C.exp|hb_position_t { hb_ot_math_get_glyph_italics_correction($font:font,$(hb_codepoint_t glyph)) }|]

math_get_glyph_top_accent_attachment :: MonadIO m => Font -> Codepoint -> m Position
math_get_glyph_top_accent_attachment font glyph = liftIO
  [C.exp|hb_position_t { hb_ot_math_get_glyph_top_accent_attachment($font:font,$(hb_codepoint_t glyph)) }|]

math_is_glyph_extended_shape :: MonadIO m => Face -> Codepoint -> m Bool
math_is_glyph_extended_shape face glyph = liftIO $
  [C.exp|hb_bool_t { hb_ot_math_is_glyph_extended_shape($face:face,$(hb_codepoint_t glyph)) }|] <&> cbool

math_get_glyph_kerning :: MonadIO m => Font -> Codepoint -> MathKern -> Position -> m Position
math_get_glyph_kerning font glyph kern correction_height = liftIO
  [C.exp|hb_position_t { hb_ot_math_get_glyph_kerning($font:font,$(hb_codepoint_t glyph),$(hb_ot_math_kern_t kern),$(hb_position_t correction_height)) }|]

math_get_min_connector_overlap :: MonadIO m => Font -> Direction -> m Position
math_get_min_connector_overlap font dir = liftIO [C.exp|hb_position_t { hb_ot_math_get_min_connector_overlap($font:font,$(hb_direction_t dir)) }|]

-- | Fetches the glyph assembly for the specified font, glyph index, and direction.
--
-- Returned are a list of glyph parts that can be used to draw the glyph and an italics-correction value
-- (if one is defined in the font).
math_get_glyph_assembly :: MonadIO m => Font -> Codepoint -> Direction -> m ([MathGlyphPart],Position)
math_get_glyph_assembly font glyph dir = liftIO $ do
    (total, retrieved, parts, italics_correction) <- go 0 8
    if total == retrieved then return (parts, italics_correction) else go 8 (total - 8) <&> \(_,_,parts2,_) -> (parts ++ parts2, italics_correction)
  where
    go start_offset requested_parts_count = alloca $ \pitalics_correction ->
      with requested_parts_count $ \pparts_count ->
        allocaArray (fromIntegral requested_parts_count) $ \pparts -> do
          total_number_of_parts <- [C.exp|unsigned int {
            hb_ot_math_get_glyph_assembly(
              $font:font,
              $(hb_codepoint_t glyph),
              $(hb_direction_t dir),
              $(unsigned int start_offset),
              $(unsigned int * pparts_count),
              $(hb_ot_math_glyph_part_t * pparts),
              $(hb_position_t * pitalics_correction)
            )
          }|]
          retrieved_parts_count <- peek pparts_count
          parts <- peekArray (fromIntegral retrieved_parts_count) pparts
          italics_correction <- peek pitalics_correction
          pure (total_number_of_parts, retrieved_parts_count, parts, italics_correction)

-- |
-- Fetches the MathGlyphConstruction for the specified font, glyph index, and
-- direction. The corresponding list of size variants is returned as a list of
-- @hb_ot_math_glyph_variant_t@ structs.
math_get_glyph_variants :: MonadIO m => Font -> Codepoint -> Direction -> m [MathGlyphVariant]
math_get_glyph_variants font glyph dir = pump 8 $ \start_offset requested_variants_count ->
  with requested_variants_count $ \pvariants_count ->
    allocaArray (fromIntegral requested_variants_count) $ \pvariants -> do
      total_number_of_variants <- [C.exp|unsigned int {
        hb_ot_math_get_glyph_variants(
          $font:font,
          $(hb_codepoint_t glyph),
          $(hb_direction_t dir),
          $(unsigned int start_offset),
          $(unsigned int * pvariants_count),
          $(hb_ot_math_glyph_variant_t * pvariants)
        )
      }|]
      retrieved_variants_count <- peek pvariants_count
      variants <- peekArray (fromIntegral retrieved_variants_count) pvariants
      pure (total_number_of_variants, retrieved_variants_count, variants)
