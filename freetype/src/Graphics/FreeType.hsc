{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_MODULE_H
#include FT_TYPES_H
#include FT_FONT_FORMATS_H

-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--

module Graphics.FreeType
(
-- * Library

  Library

, init_library

-- ** reference counting
, reference_library
, done_library

-- ** properties
, property_set
, property_get

-- ** raw library initialization
, new_uninitialized_library -- name mangled to keep users from reaching for it
, add_default_modules
, set_default_properties


-- ** library version
, library_version
, library_version_string
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH

-- * Faces

, Face
, new_face
, new_memory_face

-- ** manual reference counting
, reference_face
, done_face

-- ** Using the face
, get_font_format
, attach_file
, get_char_index
, get_first_char
, get_next_char
, get_name_index
, set_pixel_sizes
, set_transform
, load_char
, load_glyph
, has_multiple_masters

-- glyph_slot
, GlyphSlot
, face_glyph
, glyph_slot_face
-- diffs
, glyph_slot_glyph_index
, glyph_slot_generic
, glyph_slot_linearHoriAdvance
, glyph_slot_linearVertAdvance
, glyph_slot_bitmap_left
, glyph_slot_bitmap_top
, glyph_slot_num_subglyphs

-- * Math
-- ** angles

, Angle
, pattern ANGLE_PI
, pattern ANGLE_2PI
, pattern ANGLE_PI2
, pattern ANGLE_PI4
, angleDiff

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
) where

import Control.Monad.IO.Class
import Data.ByteString as ByteString
import Data.Functor ((<&>))
import Data.Int
import Data.Version
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.Storable
import Graphics.FreeType.Internal
import qualified Language.C.Inline as C
import Numeric.Fixed

C.context $ C.baseCtx <> C.bsCtx <> C.fptrCtx <> freeTypeCtx
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"
C.verbatim "#include FT_FONT_FORMATS_H"
C.include "ft.h"

--part :: ForeignPtr a => Diff a b

-- this will use fixed memory allocation functions, but allows us to avoid the FT_Init_FreeType and FT_Done_FreeType global mess.
new_face :: MonadIO m => Library -> FilePath -> Int -> m Face
new_face library path (fromIntegral -> face_index) = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error { FT_New_Face($library:library,$str:path,$(FT_Long face_index),$(FT_Face * p))}|] >>= ok
    peek p >>= foreignFace

new_memory_face :: MonadIO m => Library -> ByteString -> Int -> m Face
new_memory_face library base (fromIntegral -> face_index) = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error { FT_New_Memory_Face($library:library,$bs-ptr:base,$bs-len:base,$(FT_Long face_index),$(FT_Face * p))}|] >>= ok
    peek p >>= foreignFace

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
done_face face = liftIO $
  [C.exp|FT_Error { FT_Done_Face($face:face)}|] >>= ok

get_char_index :: MonadIO m => Face -> Word32 -> m Word32
get_char_index face c = liftIO $
  [C.exp|FT_UInt { FT_Get_Char_Index($face:face,$(FT_ULong c)) }|]

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
attach_file face path = liftIO $
  [C.exp|FT_Error { FT_Attach_File($face:face,$str:path) }|] >>= ok

set_pixel_sizes :: MonadIO m => Face -> Int -> Int -> m ()
set_pixel_sizes face (fromIntegral -> pixel_width) (fromIntegral -> pixel_height) = liftIO $
  [C.exp|FT_Error { FT_Set_Pixel_Sizes($face:face,$(FT_UInt pixel_width),$(FT_UInt pixel_height)) }|] >>= ok

get_name_index :: MonadIO m => Face -> ByteString -> m Word32
get_name_index face name = liftIO [C.exp|FT_UInt { FT_Get_Name_Index($face:face,$bs-cstr:name) }|]

load_char :: MonadIO m => Face -> Word32 -> Int32 -> m ()
load_char face char_code load_flags = liftIO $ [C.exp|FT_Error { FT_Load_Char($face:face,$(FT_ULong char_code),$(FT_Int32 load_flags)) }|] >>= ok

load_glyph :: MonadIO m => Face -> Word32 -> Int32 -> m ()
load_glyph face glyph_index load_flags = liftIO $ [C.exp|FT_Error { FT_Load_Glyph($face:face,$(FT_ULong glyph_index),$(FT_Int32 load_flags)) }|] >>= ok

face_glyph :: MonadIO m => Face -> m GlyphSlot
face_glyph face = liftIO $ childPtr face <$> [C.exp|FT_GlyphSlot { $face:face->glyph }|]

glyph_slot_face :: MonadIO m => GlyphSlot -> m Face
glyph_slot_face slot = liftIO $ childPtr slot <$> [C.exp|FT_Face { $glyph-slot:slot->face }|]

#let diff hsFrom, cTy, hsName, cField, hsTo = "%s :: Diff %s %s\n%s = Diff %lu\n{-# inline %s #-}", #hsName, #hsFrom, #hsTo, #hsName, (long) offsetof(cTy,cField), #hsName

#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_glyph_index, glyph_index, Word32
#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_generic, generic, Generic
--diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_metrics, metrics, GlyphMetrics
#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_linearHoriAdvance, linearHoriAdvance, Fixed
#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_linearVertAdvance, linearVertAdvance, Fixed
--diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_format, format, GlyphFormat
--diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_bitmap, bitmap, Bitmap
#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_bitmap_left, bitmap_left, Int32
#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_bitmap_top, bitmap_top, Int32
--diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_outline, outline, Outline
#diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_num_subglyphs, num_subglyphs, Word32
--diff GlyphSlotRec, FT_GlyphSlotRec, glyph_slot_subglyphs, subglyphs, SubGlyph

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

