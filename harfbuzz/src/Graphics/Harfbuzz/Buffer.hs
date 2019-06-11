{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language BlockArguments #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.Buffer
( Buffer

, buffer_add
, buffer_add_char
, buffer_add_latin1 -- Char8.ByteString
, buffer_add_string -- String
, buffer_add_text -- Text
, buffer_add_utf8 -- UTF8 encoded ByteString
, buffer_allocation_successful
, buffer_append
, buffer_clear_contents
, buffer_cluster_level -- statevar
, buffer_content_type -- statevar
, buffer_create
, buffer_deserialize_glyphs
, buffer_diff
, buffer_direction -- statevar
, buffer_flags -- statevar
, buffer_get_glyph_positions
, buffer_get_glyph_infos
, buffer_get_length
, buffer_guess_segment_properties
, buffer_invisible_glyph
, buffer_language -- statevar
, buffer_normalize_glyphs
, buffer_pre_allocate
, buffer_replacement_codepoint
, buffer_reset
, buffer_reverse
, buffer_reverse_clusters
, buffer_reverse_range
, buffer_script -- statevar
, buffer_segment_properties
, buffer_serialize_glyphs
, buffer_serialize_list_formats
, buffer_set_length
, buffer_set_message_func
, buffer_unicode_funcs -- statevar

, BufferFlags(..)
, BufferContentType(..)
, BufferClusterLevel(..)
, BufferSerializeFormat(..)
, buffer_serialize_format_from_string, buffer_serialize_format_to_string

, GlyphInfo -- No constructor, partial fields
  ( glyph_info_codepoint
  , glyph_info_cluster
  )
, glyph_info_get_glyph_flags
, GlyphPosition -- No constructor, partial fields
  ( glyph_position_x_advance
  , glyph_position_y_advance
  , glyph_position_x_offset
  , glyph_position_y_offset
  )
, GlyphFlags(..)

, SegmentProperties(..)
-- , (==) provides hb_segment_properties_equal
-- , hash provides hb_segment_properties_hash

, pattern BUFFER_REPLACEMENT_CODEPOINT_DEFAULT
) where

import Control.Monad.Primitive
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor ((<&>))
import Data.Primitive.StateVar
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Vector.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> C.bsCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

-- * buffers

buffer_create :: PrimMonad m => m (Buffer (PrimState m))
buffer_create = unsafeIOToPrim $ [C.exp|hb_buffer_t * { hb_buffer_create() }|] >>= foreignBuffer

buffer_diff :: PrimMonad m => Buffer (PrimState m) -> Buffer (PrimState m) -> Codepoint -> Int -> m BufferDiffFlags
buffer_diff buffer reference dottedcircle_glyph (fromIntegral -> position_fuzz) = unsafeIOToPrim
  [C.exp|hb_buffer_diff_flags_t { hb_buffer_diff($buffer:buffer,$buffer:reference,$(hb_codepoint_t dottedcircle_glyph),$(unsigned int position_fuzz)) }|]

-- | Resets the buffer to its initial status, as if it was just newly created with 'buffer_create'
buffer_reset :: PrimMonad m => Buffer (PrimState m) -> m ()
buffer_reset b = unsafeIOToPrim [C.block|void { hb_buffer_reset($buffer:b); }|]

-- | Similar to 'buffer_reset', but does not clear the Unicode functions and the replacement code point.
buffer_clear_contents :: PrimMonad m => Buffer (PrimState m) -> m ()
buffer_clear_contents b = unsafeIOToPrim [C.block|void { hb_buffer_clear_contents($buffer:b); }|]

buffer_pre_allocate :: PrimMonad m => Buffer (PrimState m) -> Int -> m Bool
buffer_pre_allocate b (fromIntegral -> size) = unsafeIOToPrim
  [C.exp|hb_bool_t { hb_buffer_pre_allocate($buffer:b,$(unsigned int size)) }|] <&> cbool

buffer_set_length :: PrimMonad m => Buffer (PrimState m) -> Int -> m Bool
buffer_set_length b (fromIntegral -> size) = unsafeIOToPrim
  [C.exp|hb_bool_t { hb_buffer_set_length($buffer:b,$(unsigned int size)) }|] <&> cbool

buffer_get_length :: PrimMonad m => Buffer (PrimState m) -> m Int
buffer_get_length b = unsafeIOToPrim $
  [C.exp|unsigned int { hb_buffer_get_length($buffer:b) }|] <&> fromIntegral

buffer_allocation_successful :: PrimMonad m => Buffer (PrimState m) -> m Bool
buffer_allocation_successful b = unsafeIOToPrim $
  [C.exp|hb_bool_t { hb_buffer_allocation_successful($buffer:b) }|] <&> cbool

buffer_reverse :: PrimMonad m => Buffer (PrimState m) -> m ()
buffer_reverse b = unsafeIOToPrim [C.block|void { hb_buffer_reverse($buffer:b); }|]

buffer_reverse_clusters :: PrimMonad m => Buffer (PrimState m) -> m ()
buffer_reverse_clusters b = unsafeIOToPrim [C.block|void { hb_buffer_reverse_clusters($buffer:b); }|]

buffer_reverse_range :: PrimMonad m => Buffer (PrimState m) -> Int -> Int -> m ()
buffer_reverse_range b (fromIntegral -> start) (fromIntegral -> end) = unsafeIOToPrim
  [C.block|void { hb_buffer_reverse_range($buffer:b,$(unsigned int start), $(unsigned int end)); }|]

buffer_add :: PrimMonad m => Buffer (PrimState m) -> Codepoint -> Int -> m ()
buffer_add buffer codepoint (fromIntegral -> cluster) = unsafeIOToPrim
  [C.block|void { hb_buffer_add($buffer:buffer,$(hb_codepoint_t codepoint),$(unsigned int cluster)); }|]

buffer_add_char :: PrimMonad m => Buffer (PrimState m) -> Char -> Int -> m ()
buffer_add_char buffer = buffer_add buffer . c2w

buffer_add_string :: PrimMonad m => Buffer (PrimState m) -> String -> Int -> Int -> m ()
buffer_add_string buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = unsafeIOToPrim $
  withCWStringLen text \(castPtr -> cwstr,fromIntegral -> len) ->
    [C.block|void {
      hb_buffer_add_utf16($buffer:buffer,$(const uint16_t * cwstr),$(int len),$(unsigned int item_offset),$(int item_length));
    }|]

buffer_add_text :: PrimMonad m => Buffer (PrimState m) -> Text -> Int -> Int -> m ()
buffer_add_text buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = unsafeIOToPrim $
  Text.withCStringLen text \(cstr,fromIntegral -> len) ->
    [C.block|void {
      hb_buffer_add_utf8($buffer:buffer,$(const char * cstr),$(int len),$(unsigned int item_offset),$(int item_length));
    }|]

buffer_add_latin1 :: PrimMonad m => Buffer (PrimState m) -> ByteString -> Int -> Int -> m ()
buffer_add_latin1 buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = unsafeIOToPrim $
  [C.block|void {
    hb_buffer_add_latin1($buffer:buffer,$bs-ptr:text,$bs-len:text,$(unsigned int item_offset),$(int item_length));
  }|]

buffer_add_utf8 :: PrimMonad m => Buffer (PrimState m) -> ByteString -> Int -> Int -> m ()
buffer_add_utf8 buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = unsafeIOToPrim $
  [C.block|void {
    hb_buffer_add_utf8($buffer:buffer,$bs-ptr:text,$bs-len:text,$(unsigned int item_offset),$(int item_length));
  }|]

buffer_append :: PrimMonad m => Buffer (PrimState m) -> Buffer (PrimState m) -> Int -> Int -> m ()
buffer_append buffer source (fromIntegral -> start) (fromIntegral -> end) = unsafeIOToPrim
  [C.block|void { hb_buffer_append($buffer:buffer,$buffer:source,$(unsigned int start),$(unsigned int end)); }|]

buffer_get_glyph_positions :: PrimMonad m => Buffer (PrimState m) -> m (Vector GlyphPosition)
buffer_get_glyph_positions b = unsafeIOToPrim $ alloca \plen -> do
  positions <- [C.exp|hb_glyph_position_t * { hb_buffer_get_glyph_positions($buffer:b,$(unsigned int * plen)) }|]
  len <- peek plen
  peekVector (fromIntegral len) positions -- we don't own the array, its valid as long as the buffer is unmodified, so don't deallocate

-- @hb_buffer_get_glyph_infos@ only gives us access to @hb_glyph_info_glyph_flags@, so just map
-- that over the list rather than trying to deal with memory management on an opaque object we know nothing about.
buffer_get_glyph_infos :: PrimMonad m => Buffer (PrimState m) -> m (Vector GlyphInfo)
buffer_get_glyph_infos b = unsafeIOToPrim $ alloca \plen -> do
  pinfos <- [C.exp|hb_glyph_info_t * { hb_buffer_get_glyph_infos($buffer:b,$(unsigned int * plen)) }|]
  len <- peek plen
  peekVector (fromIntegral len) pinfos

glyph_info_get_glyph_flags :: GlyphInfo -> GlyphFlags
glyph_info_get_glyph_flags info = [C.pure|hb_glyph_flags_t { hb_glyph_info_get_glyph_flags($glyph-info:info) }|]

buffer_guess_segment_properties :: PrimMonad m => Buffer (PrimState m) -> m ()
buffer_guess_segment_properties b = unsafeIOToPrim [C.block|void { hb_buffer_guess_segment_properties($buffer:b); }|]

buffer_normalize_glyphs :: PrimMonad m => Buffer (PrimState m) -> m ()
buffer_normalize_glyphs b = unsafeIOToPrim [C.block|void { hb_buffer_normalize_glyphs($buffer:b); }|]

-- TODO: wrapper that provides a lazy bytestring for a given window without fiddling with buffer sizes
buffer_serialize_glyphs :: PrimMonad m => Buffer (PrimState m) -> Int -> Int -> Int -> Font (PrimState m) -> BufferSerializeFormat -> BufferSerializeFlags -> m (Int, ByteString)
buffer_serialize_glyphs b (fromIntegral -> start) (fromIntegral -> end) bs@(fromIntegral -> buf_size) font format flags = unsafeIOToPrim $
  allocaBytes bs \ buf ->
    alloca \pbuf_consumed -> do
       n <- [C.exp|unsigned int {
         hb_buffer_serialize_glyphs(
           $buffer:b,
           $(unsigned int start),
           $(unsigned int end),
           $(char * buf),
           $(unsigned int buf_size),
           $(unsigned int * pbuf_consumed),
           $font:font,
           $(hb_buffer_serialize_format_t format),
           $(hb_buffer_serialize_flags_t flags)
         )
       }|] <&> fromIntegral
       buf_consumed <- peek pbuf_consumed <&> fromIntegral
       result <- ByteString.packCStringLen (buf,buf_consumed)
       return (n, result)

buffer_deserialize_glyphs :: PrimMonad m => Buffer (PrimState m) -> ByteString -> Font (PrimState m) -> BufferSerializeFormat -> m (Bool, Int)
buffer_deserialize_glyphs buffer bs font format = unsafeIOToPrim $
  alloca \pdelta -> do
    b <- [C.block|hb_bool_t {
      const char * bs = $bs-ptr:bs;
      const char * end_ptr;
      hb_bool_t result = hb_buffer_deserialize_glyphs(
        $buffer:buffer,
        bs,
        $bs-len:bs,
        &end_ptr,
        $font:font,
        $(hb_buffer_serialize_format_t format)
      );
      *($(int * pdelta)) = end_ptr - bs;
      return result;
    }|]
    delta <- peek pdelta
    pure (cbool b, fromIntegral delta)

-- | Register a callback for buffer messages
buffer_set_message_func :: PrimBase m => Buffer (PrimState m) -> (Buffer (PrimState m) -> Font (PrimState m) -> ByteString -> m ()) -> m ()
buffer_set_message_func b hfun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkBufferMessageFunc \pbuffer pfont cmsg _ -> do
    buffer <- [C.exp|hb_buffer_t * { hb_buffer_reference($(hb_buffer_t * pbuffer)) }|] >>= foreignBuffer
    font <- [C.exp|hb_font_t * { hb_font_reference($(hb_font_t * pfont)) }|] >>= foreignFont
    msg <- ByteString.packCString cmsg
    unsafePrimToIO $ hfun buffer font msg
  [C.block|void {
    hb_buffer_message_func_t f = $(hb_buffer_message_func_t f);
    hb_buffer_set_message_func($buffer:b,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

buffer_direction :: Buffer s -> StateVar s Direction
buffer_direction b = unsafeStateVar g s where
  g = [C.exp|hb_direction_t { hb_buffer_get_direction($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_direction($buffer:b,$(hb_direction_t v)); }|]

-- | Subsumes @hb_buffer_get_script@ and @hb_buffer_set_script@
buffer_script :: Buffer s -> StateVar s Script
buffer_script b = unsafeStateVar g s where
  g = [C.exp|hb_script_t { hb_buffer_get_script($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_script($buffer:b,$(hb_script_t v)); }|]

-- | Subsumes @hb_buffer_get_language@ and @hb_buffer_set_language@
buffer_language :: Buffer s -> StateVar s Language
buffer_language b = unsafeStateVar g s where
  g = Language <$> [C.exp|hb_language_t { hb_buffer_get_language($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_language($buffer:b,$language:v); }|]

-- | Subsumes @hb_buffer_get_flags@ and @hb_buffer_set_flags@
buffer_flags :: Buffer s -> StateVar s BufferFlags
buffer_flags b = unsafeStateVar g s where
  g = [C.exp|hb_buffer_flags_t { hb_buffer_get_flags($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_flags($buffer:b,$(hb_buffer_flags_t v)); }|]

-- | Subsumes @hb_buffer_get_cluster_level@ and @hb_buffer_set_cluster_level@
buffer_cluster_level :: Buffer s -> StateVar s BufferClusterLevel
buffer_cluster_level b = unsafeStateVar g s where
  g = [C.exp|hb_buffer_cluster_level_t { hb_buffer_get_cluster_level($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_cluster_level($buffer:b,$(hb_buffer_cluster_level_t v)); }|]

-- | Subsumes @hb_buffer_get_content_type@ and @hb_buffer_set_content_type@
buffer_content_type :: Buffer s -> StateVar s BufferContentType
buffer_content_type b = unsafeStateVar g s where
  g = [C.exp|hb_buffer_content_type_t { hb_buffer_get_content_type($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_content_type($buffer:b,$(hb_buffer_content_type_t v)); }|]

-- | Subsumes @hb_buffer_get_segment_properties@ and @hb_buffer_set_segment_properties@
buffer_segment_properties :: Buffer s -> StateVar s SegmentProperties
buffer_segment_properties b = unsafeStateVar g s where
  g = alloca \props -> [C.block|void { hb_buffer_get_segment_properties($buffer:b,$(hb_segment_properties_t * props)); }|] *> peek props
  s v = with v \props -> [C.block|void { hb_buffer_set_segment_properties($buffer:b,$(const hb_segment_properties_t * props)); }|]

-- | Subsumes @hb_buffer_get_unicode_funcs@ and @hb_buffer_set_unicode_funcs@
buffer_unicode_funcs :: Buffer s -> StateVar s (UnicodeFuncs s)
buffer_unicode_funcs b = unsafeStateVar g s where
  g = [C.exp|hb_unicode_funcs_t * { hb_buffer_get_unicode_funcs($buffer:b) }|] >>= foreignUnicodeFuncs
  s v = [C.block|void { hb_buffer_set_unicode_funcs($buffer:b,$unicode-funcs:v); }|]

buffer_invisible_glyph :: Buffer s -> StateVar s Codepoint
buffer_invisible_glyph b = unsafeStateVar g s where
  g = [C.exp|hb_codepoint_t { hb_buffer_get_invisible_glyph($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_invisible_glyph($buffer:b,$(hb_codepoint_t v)); }|]

-- | Note: this should be in the form of a glyph, not a codepoint.
buffer_replacement_codepoint :: Buffer s -> StateVar s Int
buffer_replacement_codepoint b = unsafeStateVar g s where
  g = [C.exp|hb_codepoint_t { hb_buffer_get_replacement_codepoint($buffer:b) }|] <&> fromEnum
  s (toEnum -> v) = [C.block|void { hb_buffer_set_replacement_codepoint($buffer:b,$(hb_codepoint_t v)); }|]
