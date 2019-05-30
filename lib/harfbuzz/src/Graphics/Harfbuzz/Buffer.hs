{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
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


import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Functor ((<&>))
import Data.StateVar
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

-- * buffers

buffer_create :: MonadIO m => m Buffer
buffer_create = liftIO $ [C.exp|hb_buffer_t * { hb_buffer_create() }|] >>= foreignBuffer

buffer_diff :: MonadIO m => Buffer -> Buffer -> Codepoint -> Int -> m BufferDiffFlags
buffer_diff buffer reference dottedcircle_glyph (fromIntegral -> position_fuzz) = liftIO
  [C.exp|hb_buffer_diff_flags_t { hb_buffer_diff($buffer:buffer,$buffer:reference,$(hb_codepoint_t dottedcircle_glyph),$(unsigned int position_fuzz)) }|]

-- | Resets the buffer to its initial status, as if it was just newly created with 'buffer_create'
buffer_reset :: MonadIO m => Buffer -> m ()
buffer_reset b = liftIO [C.block|void { hb_buffer_reset($buffer:b); }|]

-- | Similar to 'buffer_reset', but does not clear the Unicode functions and the replacement code point.
buffer_clear_contents :: MonadIO m => Buffer -> m ()
buffer_clear_contents b = liftIO [C.block|void { hb_buffer_clear_contents($buffer:b); }|]

buffer_pre_allocate :: MonadIO m => Buffer -> Int -> m Bool
buffer_pre_allocate b (fromIntegral -> size) = liftIO
  [C.exp|hb_bool_t { hb_buffer_pre_allocate($buffer:b,$(unsigned int size)) }|] <&> cbool

buffer_set_length :: MonadIO m => Buffer -> Int -> m Bool
buffer_set_length b (fromIntegral -> size) = liftIO
  [C.exp|hb_bool_t { hb_buffer_set_length($buffer:b,$(unsigned int size)) }|] <&> cbool

buffer_get_length :: MonadIO m => Buffer -> m Int
buffer_get_length b = liftIO $
  [C.exp|unsigned int { hb_buffer_get_length($buffer:b) }|] <&> fromIntegral

buffer_allocation_successful :: MonadIO m => Buffer -> m Bool
buffer_allocation_successful b = liftIO $
  [C.exp|hb_bool_t { hb_buffer_allocation_successful($buffer:b) }|] <&> cbool

buffer_reverse :: MonadIO m => Buffer -> m ()
buffer_reverse b = liftIO [C.block|void { hb_buffer_reverse($buffer:b); }|]

buffer_reverse_clusters :: MonadIO m => Buffer -> m ()
buffer_reverse_clusters b = liftIO [C.block|void { hb_buffer_reverse_clusters($buffer:b); }|]

buffer_reverse_range :: MonadIO m => Buffer -> Int -> Int -> m ()
buffer_reverse_range b (fromIntegral -> start) (fromIntegral -> end) = liftIO
  [C.block|void { hb_buffer_reverse_range($buffer:b,$(unsigned int start), $(unsigned int end)); }|]

buffer_add :: MonadIO m => Buffer -> Codepoint -> Int -> m ()
buffer_add buffer codepoint (fromIntegral -> cluster) = liftIO
  [C.block|void { hb_buffer_add($buffer:buffer,$(hb_codepoint_t codepoint),$(unsigned int cluster)); }|]

buffer_add_char :: MonadIO m => Buffer -> Char -> Int -> m ()
buffer_add_char buffer = buffer_add buffer . c2w

buffer_add_string :: MonadIO m => Buffer -> String -> Int -> Int -> m ()
buffer_add_string buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = liftIO $
  withCWStringLen text $ \(castPtr -> cwstr,fromIntegral -> len) ->
    [C.block|void {
      hb_buffer_add_utf16($buffer:buffer,$(const uint16_t * cwstr),$(int len),$(unsigned int item_offset),$(int item_length));
    }|]

buffer_add_text :: MonadIO m => Buffer -> Text -> Int -> Int -> m ()
buffer_add_text buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = liftIO $
  Text.withCStringLen text $ \(cstr,fromIntegral -> len) ->
    [C.block|void {
      hb_buffer_add_utf8($buffer:buffer,$(const char * cstr),$(int len),$(unsigned int item_offset),$(int item_length));
    }|]

buffer_add_latin1 :: MonadIO m => Buffer -> ByteString -> Int -> Int -> m ()
buffer_add_latin1 buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = liftIO $
  ByteString.useAsCStringLen text $ \(castPtr -> cstr,fromIntegral -> len) ->
    [C.block|void {
      hb_buffer_add_latin1($buffer:buffer,$(const unsigned char * cstr),$(int len),$(unsigned int item_offset),$(int item_length));
    }|]

buffer_add_utf8 :: MonadIO m => Buffer -> ByteString -> Int -> Int -> m ()
buffer_add_utf8 buffer text (fromIntegral -> item_offset) (fromIntegral -> item_length) = liftIO $
  ByteString.useAsCStringLen text $ \(cstr,fromIntegral -> len) ->
    [C.block|void {
      hb_buffer_add_utf8($buffer:buffer,$(const char * cstr),$(int len),$(unsigned int item_offset),$(int item_length));
    }|]

buffer_append :: MonadIO m => Buffer -> Buffer -> Int -> Int -> m ()
buffer_append buffer source (fromIntegral -> start) (fromIntegral -> end) = liftIO
  [C.block|void { hb_buffer_append($buffer:buffer,$buffer:source,$(unsigned int start),$(unsigned int end)); }|]

buffer_get_glyph_positions :: MonadIO m => Buffer -> m [GlyphPosition]
buffer_get_glyph_positions b = liftIO $ alloca $ \plen -> do
  positions <- [C.exp|hb_glyph_position_t * { hb_buffer_get_glyph_positions($buffer:b,$(unsigned int * plen)) }|]
  len <- peek plen
  peekArray (fromIntegral len) positions -- we don't own the array, its valid as long as the buffer is unmodified, so don't deallocate

-- @hb_buffer_get_glyph_infos@ only gives us access to @hb_glyph_info_glyph_flags@, so just map
-- that over the list rather than trying to deal with memory management on an opaque object we know nothing about.
buffer_get_glyph_infos :: MonadIO m => Buffer -> m [GlyphInfo]
buffer_get_glyph_infos b = liftIO $ alloca $ \plen -> do
  pinfos <- [C.exp|hb_glyph_info_t * { hb_buffer_get_glyph_infos($buffer:b,$(unsigned int * plen)) }|]
  len <- peek plen
  peekArray (fromIntegral len) pinfos

glyph_info_get_glyph_flags :: GlyphInfo -> GlyphFlags
glyph_info_get_glyph_flags info = [C.pure|hb_glyph_flags_t { hb_glyph_info_get_glyph_flags($glyph-info:info) }|]

buffer_guess_segment_properties :: MonadIO m => Buffer -> m ()
buffer_guess_segment_properties b = liftIO [C.block|void { hb_buffer_guess_segment_properties($buffer:b); }|]

buffer_normalize_glyphs :: MonadIO m => Buffer -> m ()
buffer_normalize_glyphs b = liftIO [C.block|void { hb_buffer_normalize_glyphs($buffer:b); }|]

-- TODO: wrapper that provides a lazy bytestring for a given window without fiddling with buffer sizes
buffer_serialize_glyphs :: MonadIO m => Buffer -> Int -> Int -> Int -> Font -> BufferSerializeFormat -> BufferSerializeFlags -> m (Int, ByteString)
buffer_serialize_glyphs b (fromIntegral -> start) (fromIntegral -> end) bs@(fromIntegral -> buf_size) font format flags = liftIO $
  allocaBytes bs $ \ buf ->
    alloca $ \pbuf_consumed -> do
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

buffer_deserialize_glyphs :: MonadIO m => Buffer -> ByteString -> Font -> BufferSerializeFormat -> m (Bool, Int)
buffer_deserialize_glyphs buffer bs font format = liftIO $
  ByteString.useAsCStringLen bs $ \(buf, fromIntegral -> buf_len) ->
    alloca $ \end_ptr -> do
      b <- [C.exp|hb_bool_t {
        hb_buffer_deserialize_glyphs(
          $buffer:buffer,
          $(const char * buf),
          $(int buf_len),
          $(const char ** end_ptr),
          $font:font,
          $(hb_buffer_serialize_format_t format)
        )
      }|] <&> cbool
      end <- peek end_ptr
      pure (b, minusPtr end buf)

-- | Register a callback for buffer messages
buffer_set_message_func :: MonadIO m => Buffer -> (Buffer -> Font -> String -> IO ()) -> m ()
buffer_set_message_func b hfun = liftIO $ do
  (castFunPtr -> f) <- mkBufferMessageFunc $ \pbuffer pfont cmsg _ -> do
    buffer <- [C.exp|hb_buffer_t * { hb_buffer_reference($(hb_buffer_t * pbuffer)) }|] >>= foreignBuffer
    font <- [C.exp|hb_font_t * { hb_font_reference($(hb_font_t * pfont)) }|] >>= foreignFont
    msg <- peekCString cmsg
    hfun buffer font msg
  [C.block|void {
    hb_buffer_message_func_t f = $(hb_buffer_message_func_t f);
    hb_buffer_set_message_func($buffer:b,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

buffer_direction :: Buffer -> StateVar Direction
buffer_direction b = StateVar g s where
  g = [C.exp|hb_direction_t { hb_buffer_get_direction($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_direction($buffer:b,$(hb_direction_t v)); }|]

-- | Subsumes @hb_buffer_get_script@ and @hb_buffer_set_script@
buffer_script :: Buffer -> StateVar Script
buffer_script b = StateVar g s where
  g = [C.exp|hb_script_t { hb_buffer_get_script($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_script($buffer:b,$(hb_script_t v)); }|]

-- | Subsumes @hb_buffer_get_language@ and @hb_buffer_set_language@
buffer_language :: Buffer -> StateVar Language
buffer_language b = StateVar g s where
  g = Language <$> [C.exp|hb_language_t { hb_buffer_get_language($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_language($buffer:b,$language:v); }|]

-- | Subsumes @hb_buffer_get_flags@ and @hb_buffer_set_flags@
buffer_flags :: Buffer -> StateVar BufferFlags
buffer_flags b = StateVar g s where
  g = [C.exp|hb_buffer_flags_t { hb_buffer_get_flags($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_flags($buffer:b,$(hb_buffer_flags_t v)); }|]

-- | Subsumes @hb_buffer_get_cluster_level@ and @hb_buffer_set_cluster_level@
buffer_cluster_level :: Buffer -> StateVar BufferClusterLevel
buffer_cluster_level b = StateVar g s where
  g = [C.exp|hb_buffer_cluster_level_t { hb_buffer_get_cluster_level($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_cluster_level($buffer:b,$(hb_buffer_cluster_level_t v)); }|]

-- | Subsumes @hb_buffer_get_content_type@ and @hb_buffer_set_content_type@
buffer_content_type :: Buffer -> StateVar BufferContentType
buffer_content_type b = StateVar g s where
  g = [C.exp|hb_buffer_content_type_t { hb_buffer_get_content_type($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_content_type($buffer:b,$(hb_buffer_content_type_t v)); }|]

-- | Subsumes @hb_buffer_get_segment_properties@ and @hb_buffer_set_segment_properties@
buffer_segment_properties :: Buffer -> StateVar SegmentProperties
buffer_segment_properties b = StateVar g s where
  g = alloca $ \props -> [C.block|void { hb_buffer_get_segment_properties($buffer:b,$(hb_segment_properties_t * props)); }|] *> peek props
  s v = with v $ \props -> [C.block|void { hb_buffer_set_segment_properties($buffer:b,$(const hb_segment_properties_t * props)); }|]

-- | Subsumes @hb_buffer_get_unicode_funcs@ and @hb_buffer_set_unicode_funcs@
buffer_unicode_funcs :: Buffer -> StateVar UnicodeFuncs
buffer_unicode_funcs b = StateVar g s where
  g = [C.exp|hb_unicode_funcs_t * { hb_buffer_get_unicode_funcs($buffer:b) }|] >>= foreignUnicodeFuncs
  s v = [C.block|void { hb_buffer_set_unicode_funcs($buffer:b,$unicode-funcs:v); }|]

buffer_invisible_glyph :: Buffer -> StateVar Codepoint
buffer_invisible_glyph b = StateVar g s where
  g = [C.exp|hb_codepoint_t { hb_buffer_get_invisible_glyph($buffer:b) }|]
  s v = [C.block|void { hb_buffer_set_invisible_glyph($buffer:b,$(hb_codepoint_t v)); }|]

-- | Note: this should be in the form of a glyph, not a codepoint.
buffer_replacement_codepoint :: Buffer -> StateVar Int
buffer_replacement_codepoint b = StateVar g s where
  g = [C.exp|hb_codepoint_t { hb_buffer_get_replacement_codepoint($buffer:b) }|] <&> fromEnum
  s (toEnum -> v) = [C.block|void { hb_buffer_set_replacement_codepoint($buffer:b,$(hb_codepoint_t v)); }|]
