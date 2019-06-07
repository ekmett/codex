{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language ForeignFunctionInterface #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-redundant-constraints #-}

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_MODULE_H
#include FT_TYPES_H
#include FT_FONT_FORMATS_H
#include "hsc-struct.h"
#include "ft.h"
#let diff hsFrom, cTy, hsName, cField, hsTo = "%s_ :: Diff (%s) (%s)\n%s_ = Diff %lu\n{-# inline %s_ #-}", #hsName, #hsFrom, #hsTo, #hsName, (long) offsetof(cTy,cField), #hsName


-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--

module Graphics.FreeType
( Error(..)
-- * Library
, Library
, LibraryRec
, init_library
, reference_library
, done_library
, property_set
, property_get
, new_uninitialized_library -- name mangled to keep users from reaching for it
, add_default_modules
, set_default_properties
, library_version
, library_version_string
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH

, Memory
, MemoryRec(..)
, AllocFunc, FreeFunc, ReallocFunc
, memory_user_
, memory_alloc_
, memory_free_
, memory_realloc_

-- * Faces

, Face
, FaceRec
, new_face
, new_memory_face
, face_family_name
, face_style_name
, reference_face
, done_face

-- ** Slots
, face_num_faces_
, face_index_
, face_flags_
, face_style_flags_
, face_num_glyphs_
, face_num_fixed_sizes_
, face_num_charmaps_
, face_ascender_
, face_descender_
, face_height_
, face_generic_
, face_units_per_EM_
, face_max_advance_width_
, face_max_advance_height_
, face_underline_position_
, face_underline_thickness_
, face_size_

-- ** Using the face
, get_font_format
, attach_file
, get_char_index
, get_first_char
, get_next_char
, get_name_index
, set_pixel_sizes
, set_transform

, LoadFlags(..)
, load_char
, load_glyph

, has_kerning
, KerningMode(..)
, get_kerning

, has_fixed_sizes
, has_color
, has_multiple_masters
, has_horizontal
, has_vertical
, has_glyph_names

, is_sfnt
, is_scalable
, is_fixed_width
, is_cid_keyed
, is_tricky
, is_named_instance
, is_variation

, Encoding(..)
, select_charmap

, CharMap
, CharMapRec(..)
, charmap_face_
, charmap_encoding_
, charmap_platform_id_
, charmap_encoding_id_

-- * Glyphs
, Glyph
, GlyphRec(..)
, glyph_advance_
, glyph_format_
, glyph_clazz_
, glyph_library_

, GlyphFormat(..)
, new_glyph
, get_glyph

, IsGlyphRec, glyph_root
, glyph_copy
, glyph_transform
, glyph_to_bitmap

, GlyphBBoxMode(..)
, glyph_get_cbox

-- ** Bitmap Glyphs
, BitmapGlyph
, BitmapGlyphRec(..)
, bitmapglyph_root_
, bitmapglyph_top_
, bitmapglyph_left_
, bitmapglyph_bitmap_

, new_bitmapglyph
-- ** Outline Glyphs
, Outline(..)
, outline_contours_
, outline_flags_
, outline_points_
, outline_tags_
, outline_n_points_
, outline_n_contours_
, OutlineFlags(..)
, OutlineGlyph
, OutlineGlyphRec(..)
, outlineglyph_root_
, outlineglyph_outline_
, new_outlineglyph

-- * GlyphSlots
, GlyphSlot
, GlyphSlotRec
, face_glyph
, glyphslot_face
-- diffs
, glyphslot_glyph_index_
, glyphslot_generic_
, glyphslot_linearHoriAdvance_
, glyphslot_linearVertAdvance_
, glyphslot_bitmap_
, glyphslot_bitmap_left_
, glyphslot_bitmap_top_
-- , glyphslot_num_subglyphs

, Bitmap(..)
, bitmap_width_
, bitmap_rows_
, bitmap_buffer_
, bitmap_pitch_
, bitmap_pixel_mode_
, bitmap_num_grays_
, bitmap_palette_mode_
, bitmap_palette_

, BitmapSize(..)
, bitmapsize_size_
, bitmapsize_height_
, bitmapsize_width_
, bitmapsize_x_ppem_
, bitmapsize_y_ppem_

, PixelMode(..)
, Pos

, RenderMode(..)
, render_glyph

, Size
, SizeRec(..)
, size_face_
, size_generic_
, size_metrics_
, size_internal_
, SizeMetrics(..)
, sizemetrics_ascender_
, sizemetrics_descender_
, sizemetrics_height_
, sizemetrics_x_scale_
, sizemetrics_y_scale_
, sizemetrics_x_ppem_
, sizemetrics_y_ppem_
, sizemetrics_max_advance_
, SizeInternalRec

, SizeRequest
, SizeRequestRec(..)
, sizerequest_type_
, sizerequest_width_
, sizerequest_height_
, sizerequest_vertResolution_
, sizerequest_horiResolution_
, SizeRequestType(..)

-- * Math
-- ** angles

, Angle
, pattern ANGLE_PI
, pattern ANGLE_2PI
, pattern ANGLE_PI2
, pattern ANGLE_PI4
, angleDiff

-- ** bounding boxes
, BBox(..)
, bbox_xMin_
, bbox_yMin_
, bbox_xMax_
, bbox_yMax_

-- ** fixed point

, Fixed(..)
-- , mulFix
-- , divFix

-- ** matices

, Matrix(..)
, matrixMultiply
, matrixInvert

-- ** vectors

, Vector(..)
, vectorTransform
-- , vectorUnit
-- , vectorRotate
-- , vectorLength
-- , vectorPolarize
-- , vectorFromPolar

, Generic(..)
, generic_data_
, generic_finalizer_
) where

import Control.Monad.IO.Class
import Data.ByteString as ByteString
import Data.ByteString.Internal as ByteString
import Data.Functor ((<&>))
import Data.Int
import Data.Version
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.StablePtr
import Foreign.Storable
import Graphics.FreeType.Internal
import GHC.ForeignPtr
import qualified Language.C.Inline as C
import Numeric.Fixed

C.context $ C.baseCtx <> C.bsCtx <> C.fptrCtx <> freeTypeCtx
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_GLYPH_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"
C.verbatim "#include FT_FONT_FORMATS_H"
C.include "ft.h"
C.include "HsFFI.h"

finalize_glyph :: FinalizerPtr GlyphRec
finalize_glyph = [C.funPtr|void finalize_face(FT_Glyph g) {
  FT_Library l = g->library;
  FT_Done_Glyph(g);
  FT_Done_Library(l);
}|]

-- you should use new_bitmap_glyph or new_outline_glyph, etc.
new_glyph :: MonadIO m => Library -> GlyphFormat -> m Glyph
new_glyph library format = liftIO $ do
  alloca $ \p -> do
    [C.exp|FT_Error {
      FT_New_Glyph($library:library,$(FT_Glyph_Format format),$(FT_Glyph * p))
    }|] >>= ok
    reference_library library
    peek p >>= newForeignPtr finalize_glyph

new_bitmapglyph :: MonadIO m => Library -> m BitmapGlyph
new_bitmapglyph library = act (inv glyph_root) <$> new_glyph library GLYPH_FORMAT_BITMAP

new_outlineglyph :: MonadIO m => Library -> m OutlineGlyph
new_outlineglyph library = act (inv glyph_root) <$> new_glyph library GLYPH_FORMAT_OUTLINE

class IsGlyphRec t where
instance IsGlyphRec BitmapGlyphRec
instance IsGlyphRec GlyphRec
instance IsGlyphRec OutlineGlyphRec

glyph_get_cbox :: (MonadIO m, IsGlyphRec t) => ForeignPtr t -> GlyphBBoxMode -> m BBox
glyph_get_cbox (act glyph_root -> glyph) (GlyphBBoxMode mode) = liftIO $
  alloca $ \p ->
    [C.block|void {
      FT_Glyph_Get_CBox($glyph:glyph,$(uint32_t mode),$(FT_BBox * p));
    }|] *> peek p

glyph_root :: IsGlyphRec t => Diff t GlyphRec
glyph_root = Diff 0

glyph_transform :: (MonadIO m, IsGlyphRec t) => ForeignPtr t -> Matrix -> Vector -> m ()
glyph_transform (act glyph_root -> glyph) m v = liftIO $
  [C.exp|FT_Error { FT_Glyph_Transform($glyph:glyph,$matrix:m,$vector:v) }|] >>= ok

get_glyph :: MonadIO m => GlyphSlot -> m Glyph
get_glyph slot = liftIO $
  alloca $ \p -> do
    [C.block|FT_Error {
      FT_GlyphSlot g = $glyph-slot:slot;
      FT_Error error = FT_Get_Glyph(g,$(FT_Glyph * p));
      if (!error) FT_Reference_Library(g->library);
      return error;
    }|] >>= ok
    peek p >>= newForeignPtr finalize_glyph

glyph_to_bitmap :: (MonadIO m, IsGlyphRec t) => ForeignPtr t -> RenderMode -> Vector -> Bool -> m BitmapGlyph
glyph_to_bitmap (act glyph_root -> glyph) mode origin (fromIntegral . fromEnum -> destroy) = liftIO $
  withForeignPtr glyph $ \pgr ->
    with pgr $ \pglyph -> do
      [C.exp|FT_Error { FT_Glyph_To_Bitmap($(FT_Glyph * pglyph),$(FT_Render_Mode mode),$vector:origin,$(FT_Bool destroy)) }|] >>= ok
      ngr <- peek pglyph
      act (inv glyph_root) <$>
        if pgr == ngr
        then pure glyph -- this is the old glyph
        else [C.block|void { FT_Reference_Library($(FT_Glyph ngr)->library); }|] *> newForeignPtr finalize_glyph ngr

glyph_copy :: (MonadIO m, IsGlyphRec t) => ForeignPtr t -> m (ForeignPtr t)
glyph_copy (act glyph_root -> glyph) = liftIO $
  alloca $ \p -> do
    [C.block|FT_Error {
      FT_Glyph g = $glyph:glyph;
      FT_Error error = FT_Glyph_Copy(g,$(FT_Glyph * p));
      if (!error) FT_Reference_Library(g->library);
      return error;
    }|] >>= ok
    result <- peek p
    act (inv glyph_root) <$> newForeignPtr finalize_glyph result

-- get_glyph :: MonadIO m =>

finalize_face :: FinalizerPtr FaceRec
finalize_face = [C.funPtr|void finalize_face(FT_Face f) {
  FT_Library lib = f->glyph->library;
  FT_Done_Face(f);
  FT_Done_Library(lib);
}|]

finalize_memory_face :: FinalizerEnvPtr () FaceRec
finalize_memory_face = [C.funPtr|void finalize_memory_face(void * stable_ptr, FT_Face f) {
  FT_Library lib = f->glyph->library;
  FT_Done_Face(f);
  hs_free_stable_ptr(stable_ptr);
  FT_Done_Library(lib);
}|]

-- | Uses the generic data facility to hold on to a reference to the library.
--
-- This ensures that we can free things in order.
new_face :: MonadIO m => Library -> FilePath -> Int -> m Face
new_face library path (fromIntegral -> i) = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error {
      FT_New_Face($library:library,$str:path,$(FT_Long i),$(FT_Face * p))
    }|] >>= ok
    reference_library library
    peek p >>= newForeignPtr finalize_face

new_memory_face :: MonadIO m => Library -> ByteString -> Int -> m Face
new_memory_face library bs@(PS bsfp _ _) (fromIntegral -> i) = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error {
      FT_New_Memory_Face($library:library,$bs-ptr:bs,$bs-len:bs,$(FT_Long i),$(FT_Face * p))
    }|] >>= ok
    reference_library library
    facePtr <- peek p
    case bsfp of
      -- 'fast' path
      ForeignPtr _ (PlainPtr mba) -> do
        -- hack together a MallocPtr that shares our MutableByteArray#
        newForeignPtr finalize_face facePtr <&> \case
          ForeignPtr addr (PlainForeignPtr finalizers) -> ForeignPtr addr (MallocPtr mba finalizers)
          _ -> error "new_memory_face: the impossible happened. newForeignPtr did not return a PlainForeignPtr"
      -- 'sane' path
      _ -> do
        stable <- newStablePtr bsfp
        newForeignPtrEnv finalize_memory_face (castStablePtrToPtr stable) facePtr

-- | Add a reference to a face
--
-- For the most part this should already be done for you through the API provided in Haskell,
-- but you may need this if you transfer the face to another library.
reference_face :: MonadIO m => Face -> m ()
reference_face face = liftIO $
  [C.exp|FT_Error { FT_Reference_Face($face:face)}|] >>= ok

-- | Remove a reference to a face
--
-- For the most part this should already be done for you through the API provided in Haskell,
-- but you may need this if you claim ownership of a face from another library.
done_face :: MonadIO m => Face -> m ()
done_face face = liftIO $ [C.exp|FT_Error { FT_Done_Face($face:face) }|] >>= ok

get_char_index :: MonadIO m => Face -> Word32 -> m Word32
get_char_index face c = liftIO [C.exp|FT_UInt { FT_Get_Char_Index($face:face,$(FT_ULong c)) }|]

-- | Returns the charmap's first code and the glyph index of the first character code, 0 if the charmap is empty.
get_first_char :: MonadIO m => Face -> m (Word32, Word32)
get_first_char face = liftIO $
  alloca $ \agindex ->
    (,) <$> [C.exp|FT_UInt { FT_Get_First_Char($face:face,$(FT_UInt * agindex)) }|] <*> peek agindex

get_next_char :: MonadIO m => Face -> Word32 -> m (Word32, Word32)
get_next_char face c = liftIO $
  alloca $ \agindex ->
    (,) <$> [C.exp|FT_UInt { FT_Get_Next_Char($face:face,$(FT_ULong c),$(FT_UInt * agindex)) }|] <*> peek agindex

-- | Normally this is used to read additional information for the face object, such as attaching an AFM file that comes
-- with a Type 1 font to get the kerning values and other metrics.
attach_file :: MonadIO m => Face -> FilePath -> m ()
attach_file face path = liftIO $ [C.exp|FT_Error { FT_Attach_File($face:face,$str:path) }|] >>= ok

set_pixel_sizes :: MonadIO m => Face -> Int -> Int -> m ()
set_pixel_sizes face (fromIntegral -> pixel_width) (fromIntegral -> pixel_height) = liftIO $
  [C.exp|FT_Error { FT_Set_Pixel_Sizes($face:face,$(FT_UInt pixel_width),$(FT_UInt pixel_height)) }|] >>= ok

get_name_index :: MonadIO m => Face -> ByteString -> m Word32
get_name_index face name = liftIO [C.exp|FT_UInt { FT_Get_Name_Index($face:face,$bs-cstr:name) }|]

load_char :: MonadIO m => Face -> Word32 -> LoadFlags -> m ()
load_char face char_code (LoadFlags load_flags) = liftIO $ [C.exp|FT_Error { FT_Load_Char($face:face,$(FT_ULong char_code),$(FT_Int32 load_flags)) }|] >>= ok

load_glyph :: MonadIO m => Face -> Word32 -> LoadFlags -> m ()
load_glyph face glyph_index (LoadFlags load_flags) = liftIO $ [C.exp|FT_Error { FT_Load_Glyph($face:face,$(FT_ULong glyph_index),$(FT_Int32 load_flags)) }|] >>= ok

-- | Returns whether the face object contains kerning data that can be accessed with 'get_kerning'
has_fixed_sizes :: MonadIO m => Face -> m Bool
has_fixed_sizes face = liftIO $ [C.exp|int { FT_HAS_FIXED_SIZES($face:face) }|] <&> (/=0)

--face_fixed_sizes :: MonadIO m => Face -> m (Maybe [BitmapSize])
--face_fixed_sizes = liftIO $ withForeignPtr

-- | Returns whether the face object contains kerning data that can be accessed with 'get_kerning'
has_kerning :: MonadIO m => Face -> m Bool
has_kerning face = liftIO $ [C.exp|int { FT_HAS_KERNING($face:face) }|] <&> (/=0)

get_kerning :: MonadIO m => Face -> Word32 -> Word32 -> KerningMode -> m Vector
get_kerning face left_glyph right_glyph (KerningMode kern_mode) = liftIO $
  alloca $ \v -> do
    [C.exp|FT_Error {
      FT_Get_Kerning(
        $face:face,
        $(FT_UInt left_glyph),
        $(FT_UInt right_glyph),
        $(FT_UInt kern_mode),
        $(FT_Vector * v)
      )
    }|] >>= ok
    peek v

has_horizontal :: MonadIO m => Face -> m Bool
has_horizontal face = liftIO $ [C.exp|int { FT_HAS_HORIZONTAL($face:face) }|] <&> (/=0)

has_vertical :: MonadIO m => Face -> m Bool
has_vertical face = liftIO $ [C.exp|int { FT_HAS_VERTICAL($face:face) }|] <&> (/=0)

has_glyph_names :: MonadIO m => Face -> m Bool
has_glyph_names face = liftIO $ [C.exp|int { FT_HAS_GLYPH_NAMES($face:face) }|] <&> (/=0)

is_sfnt :: MonadIO m => Face -> m Bool
is_sfnt face = liftIO $ [C.exp|int { FT_IS_SFNT($face:face) }|] <&> (/=0)

is_scalable :: MonadIO m => Face -> m Bool
is_scalable face = liftIO $ [C.exp|int { FT_IS_SCALABLE($face:face) }|] <&> (/=0)

is_fixed_width :: MonadIO m => Face -> m Bool
is_fixed_width face = liftIO $ [C.exp|int { FT_IS_FIXED_WIDTH($face:face) }|] <&> (/=0)

is_cid_keyed :: MonadIO m => Face -> m Bool
is_cid_keyed face = liftIO $ [C.exp|int { FT_IS_CID_KEYED($face:face) }|] <&> (/=0)

is_tricky :: MonadIO m => Face -> m Bool
is_tricky face = liftIO $ [C.exp|int { FT_IS_TRICKY($face:face) }|] <&> (/=0)

is_named_instance :: MonadIO m => Face -> m Bool
is_named_instance face = liftIO $ [C.exp|int { FT_IS_NAMED_INSTANCE($face:face) }|] <&> (/=0)

is_variation :: MonadIO m => Face -> m Bool
is_variation face = liftIO $ [C.exp|int { FT_IS_VARIATION($face:face) }|] <&> (/=0)

select_charmap :: MonadIO m => Face -> Encoding -> m ()
select_charmap face encoding = liftIO $ [C.exp|FT_Error { FT_Select_Charmap($face:face,$(FT_Encoding encoding)) }|] >>= ok

has_color :: MonadIO m => Face -> m Bool
has_color face = liftIO $ [C.exp|int { FT_HAS_COLOR($face:face) }|] <&> (/=0)

face_glyph :: MonadIO m => Face -> m GlyphSlot
face_glyph face = liftIO $ childPtr face <$> [C.exp|FT_GlyphSlot { $face:face->glyph }|]

glyphslot_face :: MonadIO m => GlyphSlot -> m Face
glyphslot_face slot = liftIO $ childPtr slot <$> [C.exp|FT_Face { $glyph-slot:slot->face }|]

#diff FaceRec, FT_FaceRec, face_num_faces, num_faces, Int32
#diff FaceRec, FT_FaceRec, face_index, face_index, Int32
#diff FaceRec, FT_FaceRec, face_flags, face_flags, Int32
#diff FaceRec, FT_FaceRec, face_style_flags, style_flags, Int32
#diff FaceRec, FT_FaceRec, face_num_glyphs, num_glyphs, Int32
#diff FaceRec, FT_FaceRec, face_num_fixed_sizes, num_fixed_sizes, Int32
#diff FaceRec, FT_FaceRec, face_num_charmaps, num_charmaps, Int32
#diff FaceRec, FT_FaceRec, face_ascender, ascender, Int16
#diff FaceRec, FT_FaceRec, face_descender, descender, Int16
#diff FaceRec, FT_FaceRec, face_height, height, Int16
#diff FaceRec, FT_FaceRec, face_units_per_EM, units_per_EM, Int16
#diff FaceRec, FT_FaceRec, face_max_advance_width, max_advance_width, Int16
#diff FaceRec, FT_FaceRec, face_max_advance_height, max_advance_height, Int16
#diff FaceRec, FT_FaceRec, face_underline_position, underline_position, Int16
#diff FaceRec, FT_FaceRec, face_underline_thickness, underline_thickness, Int16
#diff FaceRec, FT_FaceRec, face_size, size, Size
--diff FaceRec, FT_RaceRec, face_available_sizes, available_sizes, Ptr BitmapSize
-- FT_GlyphSlot      glyph;
-- FT_CharMap        charmap;

#diff FaceRec, FT_FaceRec, face_generic, generic, Generic

face_family_name :: MonadIO m => Face -> m String
face_family_name face = liftIO $ [C.exp|const char * { $face:face->family_name }|] >>= peekCString

face_style_name :: MonadIO m => Face -> m String
face_style_name face = liftIO $ [C.exp|const char * { $face:face->style_name }|] >>= peekCString

#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_glyph_index, glyph_index, Word32
#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_generic, generic, Generic
--diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_metrics, metrics, GlyphMetrics
#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_linearHoriAdvance, linearHoriAdvance, Fixed
#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_linearVertAdvance, linearVertAdvance, Fixed
--diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_format, format, GlyphFormat
#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_bitmap, bitmap, Bitmap
#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_bitmap_left, bitmap_left, Int32
#diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_bitmap_top, bitmap_top, Int32
--diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_outline, outline, Outline
-- #diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_num_subglyphs, num_subglyphs, Word32
--diff GlyphSlotRec, FT_GlyphSlotRec, glyphslot_subglyphs, subglyphs, SubGlyph -- "currently internal to freetype"

-- | This is a suitable form for use as an X11 @FONT_PROPERTY@.
--
-- Possible values are @"TrueType"@, @"Type 1"@, @"BDF"@, @"PCF"@, @"Type 42"@, @"CID Type 1"@, @"CFF"@, @"PFR"@, and @"Windows FNT"@.
get_font_format :: MonadIO m => Face -> m ByteString
get_font_format face = liftIO $
  [C.exp|const char * { FT_Get_Font_Format($face:face) }|] >>= ByteString.packCString

has_multiple_masters :: MonadIO m => Face -> m Bool
has_multiple_masters face = liftIO $
  [C.exp|int { FT_HAS_MULTIPLE_MASTERS($face:face) }|] <&> (/=0)

set_transform :: MonadIO m => Face -> Matrix -> Vector -> m ()
set_transform face m v = liftIO [C.block|void { FT_Set_Transform($face:face,$matrix:m,$vector:v); }|]

-- * Library

-- this will use fixed memory allocation functions, but allows us to avoid the FT_Init_FreeType and FT_Done_FreeType global mess.
new_uninitialized_library :: MonadIO m => m Library
new_uninitialized_library = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error { FT_New_Library(&hs_memory,$(FT_Library * p))}|] >>= ok
    peek p >>= foreignLibrary

add_default_modules :: MonadIO m => Library -> m ()
add_default_modules library = liftIO [C.block|void { FT_Add_Default_Modules($fptr-ptr:(FT_Library library)); }|]

set_default_properties :: MonadIO m => Library -> m ()
set_default_properties library = liftIO [C.block|void { FT_Set_Default_Properties($fptr-ptr:(FT_Library library)); }|]

init_library :: MonadIO m => m Library
init_library = liftIO $ do
  l <- new_uninitialized_library
  add_default_modules l
  set_default_properties l
  return l

-- | Add a reference to a library.
--
-- For the most part this should already be done for you through the API provided in Haskell.
reference_library :: MonadIO m => Library -> m ()
reference_library library = liftIO $ [C.exp|FT_Error { FT_Reference_Library($fptr-ptr:(FT_Library library))}|] >>= ok

-- | Remove a reference to a library, destroying the object if none remain.
--
-- For the most part this should already be done for you through the API provided in Haskell.
done_library :: MonadIO m => Library -> m ()
done_library library = liftIO $ [C.exp|FT_Error { FT_Done_Library($fptr-ptr:(FT_Library library))}|] >>= ok

-- | Useful when dynamic linking, as we can't rely on the patterns above which were determined at compile time.
library_version  :: MonadIO m => Library -> m Version
library_version library = liftIO $ allocaArray 3 $ \ver -> do
  [C.block|void {
     FT_Int * ver = $(FT_Int * ver);
     FT_Library_Version($fptr-ptr:(FT_Library library),ver,ver+1,ver+2);
  }|]
  a <- peek ver
  b <- peek (advancePtr ver 1)
  c <- peek (advancePtr ver 2)
  pure $ makeVersion [fromIntegral a, fromIntegral b, fromIntegral c]

library_version_string :: MonadIO m => Library -> m String
library_version_string library = showVersion <$> library_version library

property_set :: MonadIO m => Library -> ByteString -> ByteString -> Ptr a -> m ()
property_set library module_name property_name (castPtr -> value) = liftIO $
  [C.exp|FT_Error { FT_Property_Set($fptr-ptr:(FT_Library library),$bs-cstr:module_name,$bs-cstr:property_name,$(void * value))}|] >>= ok

property_get :: MonadIO m => Library -> ByteString -> ByteString -> Ptr a -> m ()
property_get library module_name property_name (castPtr -> value) = liftIO $
  [C.exp|FT_Error { FT_Property_Get($fptr-ptr:(FT_Library library),$bs-cstr:module_name,$bs-cstr:property_name,$(void * value))}|] >>= ok

render_glyph :: MonadIO m => GlyphSlot -> RenderMode -> m ()
render_glyph slot render_mode = liftIO $ [C.exp|FT_Error { FT_Render_Glyph($glyph-slot:slot,$(FT_Render_Mode render_mode)) }|] >>= ok
