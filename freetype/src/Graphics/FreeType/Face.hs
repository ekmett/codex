{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.FreeType.Face
( Face
, new_face
, new_memory_face

-- * manual reference counting
, reference_face
, done_face

-- * Using the face
, get_font_format
, attach_file
, get_char_index
, get_first_char
, get_next_char
, get_name_index
, set_pixel_sizes
, load_char
, has_multiple_masters
) where

import Data.ByteString as ByteString
import Data.Functor ((<&>))
import Data.Int
import Data.Word
import Control.Monad.IO.Class
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Language.C.Inline as C
import Graphics.FreeType.Internal

C.context $ C.baseCtx <> C.bsCtx <> C.fptrCtx <> freeTypeCtx
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"
C.verbatim "#include FT_FONT_FORMATS_H"
C.include "ft.h"

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
  [C.exp|FT_Error { FT_Reference_Face($fptr-ptr:(FT_Face face))}|] >>= ok

-- | Remove a reference to a face
--
-- For the most part this should already be done for you through the API provided in Haskell,
-- but you may need this if you claim ownership of a face from another library.
done_face :: MonadIO m => Face -> m ()
done_face face = liftIO $
  [C.exp|FT_Error { FT_Done_Face($fptr-ptr:(FT_Face face))}|] >>= ok

get_char_index :: MonadIO m => Face -> Word32 -> m Word32
get_char_index face c = liftIO $
  [C.exp|FT_UInt { FT_Get_Char_Index($fptr-ptr:(FT_Face face),$(FT_ULong c)) }|]

-- | Returns the charmap's first code and the glyph index of the first character code, 0 if the charmap is empty.
get_first_char :: MonadIO m => Face -> m (Word32, Word32)
get_first_char face = liftIO $
  alloca $ \agindex ->
    (,) <$> [C.exp|FT_UInt { FT_Get_First_Char($fptr-ptr:(FT_Face face),$(FT_UInt * agindex)) }|] <*> peek agindex

get_next_char :: MonadIO m => Face -> Word32 -> m (Word32, Word32)
get_next_char face c = liftIO $
  alloca $ \agindex ->
    (,) <$> [C.exp|FT_UInt { FT_Get_Next_Char($fptr-ptr:(FT_Face face),$(FT_ULong c),$(FT_UInt * agindex)) }|] <*> peek agindex

-- | Normally this is used to read additional information for the face object, such as attaching an AFM file that comes
-- with a Type 1 font to get the kerning values and other metrics.
attach_file :: MonadIO m => Face -> FilePath -> m ()
attach_file face path = liftIO $
  [C.exp|FT_Error { FT_Attach_File($fptr-ptr:(FT_Face face),$str:path) }|] >>= ok

set_pixel_sizes :: MonadIO m => Face -> Int -> Int -> m ()
set_pixel_sizes face (fromIntegral -> pixel_width) (fromIntegral -> pixel_height) = liftIO $
  [C.exp|FT_Error { FT_Set_Pixel_Sizes($fptr-ptr:(FT_Face face),$(FT_UInt pixel_width),$(FT_UInt pixel_height)) }|] >>= ok

get_name_index :: MonadIO m => Face -> ByteString -> m Word32
get_name_index face name = liftIO [C.exp|FT_UInt { FT_Get_Name_Index($fptr-ptr:(FT_Face face),$bs-cstr:name) }|]

load_char :: MonadIO m => Face -> Word32 -> Int32 -> m ()
load_char face char_code load_flags = liftIO $ [C.exp|FT_Error { FT_Load_Char($fptr-ptr:(FT_Face face),$(FT_ULong char_code),$(FT_Int32 load_flags)) }|] >>= ok

-- | This is a suitable form for use as an X11 @FONT_PROPERTY@.
--
-- Possible values are @"TrueType"@, @"Type 1"@, @"BDF"@, @"PCF"@, @"Type 42"@, @"CID Type 1"@, @"CFF"@, @"PFR"@, and @"Windows FNT"@.
get_font_format :: MonadIO m => Face -> m ByteString
get_font_format face = liftIO $
  [C.exp|const char * { FT_Get_Font_Format($fptr-ptr:(FT_Face face)) }|] >>= ByteString.packCString

has_multiple_masters :: MonadIO m => Face -> m Bool
has_multiple_masters face = liftIO $
  [C.exp|int { FT_HAS_MULTIPLE_MASTERS($fptr-ptr:(FT_Face face)) }|] <&> (/=0)
