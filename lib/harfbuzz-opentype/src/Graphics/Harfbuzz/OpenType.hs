{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wno-incomplete-uni-patterns #-}
-- |
module Graphics.Harfbuzz.OpenType
( font_set_funcs
-- * Languages and Scripts
, tag_to_language
, tag_to_script
, tags_from_script_and_language
, tags_to_script_and_language
-- * Names
, Name(..)
, name_list_names
, name_get
-- * Color
, Color(..)
, ColorLayer(..)
-- ** svg
, color_has_svg
, color_glyph_reference_svg
-- ** png
, color_has_png
, color_glyph_reference_png
-- ** layered glyphs
, color_has_palettes
, color_palette_get_count
, color_palette_get_name_id
, color_palette_get_flags
, color_palette_get_colors
, color_palette_color_get_name_id
, color_has_layers
, color_glyph_get_layers
-- * Layout
, LayoutGlyphClass(..)
, pattern TAG_BASE
, pattern TAG_GDEF
, pattern TAG_GPOS
, pattern TAG_GSUB
, pattern TAG_JSTF
, pattern TAG_DEFAULT_LANGUAGE
, pattern TAG_DEFAULT_SCRIPT
, layout_collect_features
, layout_collect_lookups
, layout_feature_get_characters
, layout_feature_get_lookups
-- ...
-- * Math
, MathConstant(..)
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
-- * Shaping
, shape_glyphs_closure
-- * Variation Axes
, pattern TAG_VAR_AXIS_ITALIC
, pattern TAG_VAR_AXIS_OPTICAL_SIZE
, pattern TAG_VAR_AXIS_SLANT
, pattern TAG_VAR_AXIS_WIDTH
, pattern TAG_VAR_AXIS_WEIGHT
, var_has_data
, var_get_axis_count
, var_get_axis_infos
, var_find_axis_info
, var_get_named_instance_count
, var_named_instance_get_subfamily_name_id
, var_named_instance_get_postscript_name_id
, var_normalize_variations
, var_normalize_coords
) where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Prim (copyAddrToByteArray#)
import GHC.Ptr (Ptr(Ptr))
import GHC.Types (Int(I#))
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- | Use openType fonts with 'Graphics.Harfbuzz.Shape.shape'.
--
-- Note that fonts returned by 'Graphics.Harfbuzz.Font.font_create' default to using these functions, so most clients would never need to call this function directly.

font_set_funcs :: MonadIO m => Font -> m ()
font_set_funcs font = liftIO [C.block|void { hb_ot_font_set_funcs($font:font); }|]

-- * Languages and Scripts

tag_to_script :: Tag -> Script
tag_to_script tag =[C.pure|hb_script_t { hb_ot_tag_to_script($(hb_tag_t tag)) }|]

tag_to_language :: Tag -> Language
tag_to_language tag = Language [C.pure|hb_language_t { hb_ot_tag_to_language($(hb_tag_t tag)) }|]

tags_from_script_and_language :: Script -> Language -> ([Tag],[Tag])
tags_from_script_and_language script language = unsafeLocalState $
  allocaArray 256 $ \pscripts ->
    withArray [128,128] $ \pscript_count -> do
      let planguages = advancePtr pscripts 128
          planguage_count = advancePtr pscript_count 1
      [C.block|void { hb_ot_tags_from_script_and_language( $(hb_script_t script), $language:language, $(unsigned int * pscript_count), $(hb_tag_t * pscripts), $(unsigned int * planguage_count), $(hb_tag_t * planguages)); }|]
      nscripts <- fromIntegral <$> peek pscript_count
      nlanguages <- fromIntegral <$> peek planguage_count
      (,) <$> peekArray nscripts pscripts <*> peekArray nlanguages planguages

tags_to_script_and_language :: Tag -> Tag -> (Script,Language)
tags_to_script_and_language script_tag language_tag = unsafeLocalState $
  alloca $ \pscript -> alloca $ \ planguage -> do
    [C.block|void { hb_ot_tags_to_script_and_language( $(hb_tag_t script_tag),$(hb_tag_t language_tag),$(hb_script_t * pscript),$(hb_language_t * planguage)); }|]
    (,) <$> peek pscript <*> (Language <$> peek planguage)

-- * Color

color_has_svg :: MonadIO m => Face -> m Bool
color_has_svg face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_svg($face:face) }|] <&> cbool

color_glyph_reference_svg :: MonadIO m => Face -> Codepoint -> m Blob
color_glyph_reference_svg face glyph = liftIO $ [C.exp|hb_blob_t * { hb_ot_color_glyph_reference_svg($face:face,$(hb_codepoint_t glyph)) }|] >>= foreignBlob

color_has_png :: MonadIO m => Face -> m Bool
color_has_png face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_png($face:face) }|] <&> cbool

-- | Why does this take a font and the svg option take a face?! *sigh*
color_glyph_reference_png :: MonadIO m => Font -> Codepoint -> m Blob
color_glyph_reference_png font glyph = liftIO $ [C.exp|hb_blob_t * { hb_ot_color_glyph_reference_png($font:font,$(hb_codepoint_t glyph)) }|] >>= foreignBlob

color_has_palettes :: MonadIO m => Face -> m Bool
color_has_palettes face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_palettes($face:face) }|] <&> cbool

color_palette_get_count :: MonadIO m => Face -> m Int
color_palette_get_count face = liftIO $ [C.exp|unsigned int { hb_ot_color_palette_get_count($face:face) }|] <&> fromIntegral

color_palette_get_name_id :: MonadIO m => Face -> Int -> m Name
color_palette_get_name_id face (fromIntegral -> palette_index) = liftIO
  [C.exp|hb_ot_name_id_t { hb_ot_color_palette_get_name_id($face:face,$(unsigned int palette_index)) }|]

color_palette_color_get_name_id :: MonadIO m => Face -> Int -> m Name
color_palette_color_get_name_id face (fromIntegral -> color_index) = liftIO
  [C.exp|hb_ot_name_id_t { hb_ot_color_palette_color_get_name_id($face:face,$(unsigned int color_index)) }|]

color_palette_get_flags :: MonadIO m => Face -> Int -> m ColorPaletteFlags
color_palette_get_flags face (fromIntegral -> palette_index) = liftIO
  [C.exp|hb_ot_color_palette_flags_t { hb_ot_color_palette_get_flags($face:face,$(unsigned int palette_index)) }|]

-- still missing from Data.Primitive.PrimArray as of 0.7
copyPtrToPrimArray :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Ptr a -> Int -> m ()
copyPtrToPrimArray (MutablePrimArray mba) ((I# (sizeOf# @a undefined) *) -> I# offset) (Ptr addr) ((I# (sizeOf# @a undefined) *) -> I# len) =
  primitive_ $ \s -> copyAddrToByteArray# addr mba offset len s

color_palette_get_colors :: MonadIO m => Face -> Int -> m (PrimArray Color)
color_palette_get_colors face (fromIntegral -> palette_index) = liftIO $
  withSelf face $ \f -> do
    n@(fromIntegral -> i) <- [C.exp|unsigned int { hb_ot_color_palette_get_colors($(hb_face_t * f),$(unsigned int palette_index),0,0,0) }|]
    allocaArray i $ \pcolors ->
      with n $ \pn -> do
        _ <- [C.exp|unsigned int { hb_ot_color_palette_get_colors($(hb_face_t * f),$(unsigned int palette_index),0,$(unsigned int * pn),$(hb_color_t * pcolors)) }|]
        mpa <- newPrimArray i
        copyPtrToPrimArray mpa 0 pcolors i
        unsafeFreezePrimArray mpa

color_has_layers :: MonadIO m => Face -> m Bool
color_has_layers face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_layers($face:face) }|] <&> cbool

color_glyph_get_layers_ :: Face -> Codepoint -> Int -> Int -> IO (Int,Int,[ColorLayer])
color_glyph_get_layers_ face glyph (fromIntegral -> start_offset) requested_layers_count = liftIO $
    with (fromIntegral requested_layers_count) $ \players_count ->
      allocaArray requested_layers_count $ \players -> do
        total_number_of_layers <- [C.exp|unsigned int {
          hb_ot_color_glyph_get_layers(
            $face:face,
            $(hb_codepoint_t glyph),
            $(unsigned int start_offset),
            $(unsigned int * players_count),
            $(hb_ot_color_layer_t * players)
          )
        }|] <&> fromIntegral
        retrieved_layers_count <- fromIntegral <$> peek players_count
        layers <- peekArray retrieved_layers_count players
        pure (total_number_of_layers, retrieved_layers_count, layers)


color_glyph_get_layers :: MonadIO m => Face -> Codepoint -> m [ColorLayer]
color_glyph_get_layers face glyph = liftIO $ do
  (total, retrieved, layers) <- color_glyph_get_layers_ face glyph 0 8
  if total == retrieved
  then return layers
  else color_glyph_get_layers_ face glyph 8 (total - 8) <&> \(_,_,layers2) -> layers ++ layers2

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

math_get_glyph_assembly_ :: Font -> Codepoint -> Direction -> Int -> Int -> IO (Int, Int, [MathGlyphPart], Position)
math_get_glyph_assembly_ font glyph dir (fromIntegral -> start_offset) requested_parts_count = liftIO $
  alloca $ \pitalics_correction ->
    with (fromIntegral requested_parts_count) $ \pparts_count ->
      allocaArray requested_parts_count $ \pparts -> do
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
        }|] <&> fromIntegral
        retrieved_parts_count <- fromIntegral <$> peek pparts_count
        parts <- peekArray retrieved_parts_count pparts
        italics_correction <- peek pitalics_correction
        pure (total_number_of_parts, retrieved_parts_count, parts, italics_correction)

-- | Fetches the glyph assembly for the specified font, glyph index, and direction.
--
-- Returned are a list of glyph parts that can be used to draw the glyph and an italics-correction value
-- (if one is defined in the font).
math_get_glyph_assembly :: MonadIO m => Font -> Codepoint -> Direction -> m ([MathGlyphPart],Position)
math_get_glyph_assembly font glyph dir = liftIO $ do
  (total, retrieved, parts, italics_correction) <- math_get_glyph_assembly_ font glyph dir 0 8
  if total == retrieved
  then return (parts, italics_correction)
  else math_get_glyph_assembly_ font glyph dir 8 (total - 8) <&> \(_,_,parts2,_) -> (parts ++ parts2, italics_correction)

math_get_glyph_variants_ :: Font -> Codepoint -> Direction -> Int -> Int -> IO (Int, Int, [MathGlyphVariant])
math_get_glyph_variants_ font glyph dir (fromIntegral -> start_offset) requested_variants_count = liftIO $
    with (fromIntegral requested_variants_count) $ \pvariants_count ->
      allocaArray requested_variants_count $ \pvariants -> do
        total_number_of_variants <- [C.exp|unsigned int {
          hb_ot_math_get_glyph_variants(
            $font:font,
            $(hb_codepoint_t glyph),
            $(hb_direction_t dir),
            $(unsigned int start_offset),
            $(unsigned int * pvariants_count),
            $(hb_ot_math_glyph_variant_t * pvariants)
          )
        }|] <&> fromIntegral
        retrieved_variants_count <- fromIntegral <$> peek pvariants_count
        variants <- peekArray retrieved_variants_count pvariants
        pure (total_number_of_variants, retrieved_variants_count, variants)

-- |
-- Fetches the MathGlyphConstruction for the specified font, glyph index, and
-- direction. The corresponding list of size variants is returned as a list of
-- @hb_ot_math_glyph_variant_t@ structs.
math_get_glyph_variants :: MonadIO m => Font -> Codepoint -> Direction -> m [MathGlyphVariant]
math_get_glyph_variants font glyph dir = liftIO $ do
  (total, retrieved, variants) <- math_get_glyph_variants_ font glyph dir 0 8
  if total == retrieved
  then return variants
  else math_get_glyph_variants_ font glyph dir 8 (total - 8) <&> \(_,_,variants2) -> variants ++ variants2

-- * Names

name_list_names :: MonadIO m => Face -> m [NameEntry]
name_list_names face = liftIO $
  alloca $ \plen -> do
    entries <- [C.exp|const hb_ot_name_entry_t * { hb_ot_name_list_names ($face:face,$(unsigned int * plen)) }|]
    len <- peek plen
    peekArray (fromIntegral len) entries -- do not free

name_get_ :: Face -> Name -> Language -> Int -> IO (Either Int String)
name_get_ face name language buflen = do
  with (fromIntegral buflen) $ \pbuflen -> do
    allocaBytes buflen $ \buf -> do
      full_len <- fromIntegral <$> [C.exp|unsigned int { hb_ot_name_get_utf32($face:face,$(hb_ot_name_id_t name),$language:language,$(unsigned int * pbuflen),$(uint32_t * buf))}|]
      if full_len > buflen
      then pure $ Left full_len
      else Right <$> do
        actual_len <- peek pbuflen
        peekArray (fromIntegral actual_len) (castPtr buf)

name_get :: MonadIO m => Face -> Name -> Language -> m (Maybe String)
name_get face name language = liftIO $
  name_get_ face name language 1024 >>= \case
    Left n -> name_get_ face name language n >>= \case -- slow path
      Left n' -> fail $ "ot_name_get: multiple fetches failed: actual length: " ++ show n'
      Right s -> pure $ Just s
    Right s -> pure $ Just s

shape_glyphs_closure :: MonadIO m => Font -> Buffer -> [Feature] -> Set -> m ()
shape_glyphs_closure font buffer features glyphs = liftIO $
  withArrayLen features $ \ (fromIntegral -> num_features) pfeatures ->
    [C.block|void { hb_ot_shape_glyphs_closure( $font:font, $buffer:buffer, $(const hb_feature_t * pfeatures), $(unsigned int num_features), $set:glyphs); }|]

-- * Layout

-- | Fetches a list of all feature-lookup indexes in the specified face's GSUB table or GPOS table, underneath the specified scripts,
-- languages, and features. If no list of scripts is provided, all scripts will be queried. If no list of languages is provided, all
-- languages will be queried. If no list of features is provided, all features will be queried.
layout_collect_lookups :: MonadIO m => Face -> Tag -> Maybe [Tag] -> Maybe [Tag] -> Maybe [Tag] -> Set -> m ()
layout_collect_lookups face table_tag scripts languages features lookup_indices = liftIO $
  [C.block|void { hb_ot_layout_collect_lookups( $face:face, $(hb_tag_t table_tag), $maybe-tags:scripts, $maybe-tags:languages, $maybe-tags:features, $set:lookup_indices); }|]

-- | Fetches a list of all feature indexes in the specified face's GSUB table or GPOS table, underneath the specified scripts,
-- languages, and features. If no list of scripts is provided, all scripts will be queried. If no list of languages is provided,
-- all languages will be queried. If no list of features is provided, all features will be queried.
layout_collect_features :: MonadIO m => Face -> Tag -> Maybe [Tag] -> Maybe [Tag] -> Maybe [Tag] -> Set -> m ()
layout_collect_features face table_tag scripts languages features feature_indices = liftIO $
  [C.block|void { hb_ot_layout_collect_features( $face:face, $(hb_tag_t table_tag), $maybe-tags:scripts, $maybe-tags:languages, $maybe-tags:features, $set:feature_indices); }|]

-- | Fetches a list of the characters defined as having a variant under the specified "Character Variant" ("cvXX") feature tag.
--
-- Note: If the length of the list of codepoints is equal to the supplied char_count then there is a chance that there where
-- more characters defined under the feature tag than were returned. This function can be called with incrementally larger
-- start_offset until the char_count output value is lower than its input value, or the size of the characters array can be increased.
layout_feature_get_characters :: MonadIO m => Face -> Tag -> Int -> Int -> Int -> m (Int, [Codepoint])
layout_feature_get_characters face table_tag (fromIntegral -> feature_index) (fromIntegral -> start_offset) char_count = liftIO $
  allocaArray char_count $ \ pcharacters ->
    with (fromIntegral char_count) $ \pchar_count -> do
      n <- [C.exp|unsigned int { hb_ot_layout_feature_get_characters( $face:face, $(hb_tag_t table_tag), $(unsigned int feature_index), $(unsigned int start_offset), $(unsigned int * pchar_count), $(hb_codepoint_t * pcharacters)) }|]
      actual_char_count <- peek pchar_count
      cs <- peekArray (fromIntegral actual_char_count) pcharacters
      pure (fromIntegral n, cs)

-- | Fetches a list of all lookups enumerated for the specified feature, in the specified face's GSUB table or GPOS table.
-- The list returned will begin at the offset provided.
layout_feature_get_lookups :: MonadIO m => Face -> Tag -> Int -> Int -> Int -> m (Int, [Int])
layout_feature_get_lookups face table_tag (fromIntegral -> feature_index) (fromIntegral -> start_offset) lookup_count = liftIO $
  allocaArray lookup_count $ \plookup_indices ->
    with (fromIntegral lookup_count) $ \plookup_count -> do
      n <- [C.exp|unsigned int { hb_ot_layout_feature_get_lookups( $face:face, $(hb_tag_t table_tag), $(unsigned int feature_index), $(unsigned int start_offset), $(unsigned int * plookup_count), $(unsigned int * plookup_indices)) }|]
      actual_lookup_count <- peek plookup_count
      is <- peekArray (fromIntegral actual_lookup_count) plookup_indices
      pure (fromIntegral n, fromIntegral <$> is)

-- * Variation axes

var_has_data :: MonadIO m => Face -> m Bool
var_has_data face = liftIO $ [C.exp|hb_bool_t { hb_ot_var_has_data($face:face) }|] <&> cbool

var_get_axis_count :: MonadIO m => Face -> m Int
var_get_axis_count face = liftIO $ [C.exp|unsigned int { hb_ot_var_get_axis_count($face:face) }|] <&> fromIntegral

var_get_axis_infos_ :: Face -> Int -> Int -> IO (Int, Int, [VarAxisInfo])
var_get_axis_infos_ face (fromIntegral -> start_offset) requested_axes_count = do
  with (fromIntegral requested_axes_count) $ \paxes_count ->
    allocaArray requested_axes_count $ \paxes -> do
      total_axes <- [C.exp|unsigned int { hb_ot_var_get_axis_infos($face:face,$(unsigned int start_offset),$(unsigned int * paxes_count),$(hb_ot_var_axis_info_t * paxes)) }|] <&> fromIntegral
      retrieved_axes <- fromIntegral <$> peek paxes_count
      axes <- peekArray retrieved_axes paxes
      pure (total_axes, retrieved_axes, axes)

var_get_axis_infos :: MonadIO m => Face -> m [VarAxisInfo]
var_get_axis_infos face = liftIO $ do
  (tot, ret, axes) <- var_get_axis_infos_ face 0 8
  if tot == ret
  then pure axes
  else var_get_axis_infos_ face 8 (tot - 8) <&> \(_,_,axes2) -> axes ++ axes2

var_find_axis_info :: MonadIO m => Face -> Tag -> m (Maybe VarAxisInfo)
var_find_axis_info face axis_tag = liftIO $
  alloca $ \paxis_info -> do
    b <- [C.exp|hb_bool_t { hb_ot_var_find_axis_info($face:face,$(hb_tag_t axis_tag),$(hb_ot_var_axis_info_t * paxis_info)) }|]
    if cbool b then Just <$> peek paxis_info else pure Nothing

var_get_named_instance_count :: MonadIO m => Face -> m Int
var_get_named_instance_count face = liftIO $ [C.exp|unsigned int { hb_ot_var_get_named_instance_count($face:face) }|] <&> fromIntegral

var_named_instance_get_subfamily_name_id :: MonadIO m => Face -> Int -> m Name
var_named_instance_get_subfamily_name_id face (fromIntegral -> instance_index) = liftIO [C.exp|hb_ot_name_id_t { hb_ot_var_named_instance_get_subfamily_name_id($face:face,$(unsigned int instance_index)) }|]

var_named_instance_get_postscript_name_id :: MonadIO m => Face -> Int -> m Name
var_named_instance_get_postscript_name_id face (fromIntegral -> instance_index) = liftIO [C.exp|hb_ot_name_id_t { hb_ot_var_named_instance_get_postscript_name_id($face:face,$(unsigned int instance_index)) }|]

var_normalize_variations :: MonadIO m => Face -> [Variation] -> m [Int]
var_normalize_variations face variations = liftIO $ do
  n@(fromIntegral -> num_coords) <- var_get_axis_count face
  withArrayLen variations $ \ (fromIntegral -> variations_length) pvariations ->
    allocaArray n $ \ pcoords -> do
      [C.block|void {
        hb_ot_var_normalize_variations($face:face,$(hb_variation_t * pvariations),$(unsigned int variations_length),$(int * pcoords),$(unsigned int num_coords));
      }|]
      fmap fromIntegral <$> peekArray n pcoords

var_normalize_coords :: (MonadIO m, Traversable f) => Face -> f Float -> m (f Int)
var_normalize_coords face design = liftIO $
  withArrayLen (coerce $ toList design) $ \len@(fromIntegral -> n) pdesign_coords ->
    allocaArray len $ \pnormalized_coords -> do
      [C.block|void { hb_ot_var_normalize_coords($face:face,$(unsigned int n),$(const float * pdesign_coords),$(int * pnormalized_coords)); }|]
      let step _ = state $ \ (x:xs) -> (fromIntegral x,xs)
      peekArray len pnormalized_coords <&> evalState (traverse step design)
