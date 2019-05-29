{-# language LambdaCase #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
module Graphics.Harfbuzz
( Blob
, blob_copy_writable_or_fail
, blob_create
, blob_create_from_file
, blob_create_sub_blob
, blob_get_length
, blob_is_immutable
, blob_make_immutable

, withBlobData
, withBlobDataWritable

, Buffer
, BufferFlags(..)
, BufferContentType(..)
, BufferClusterLevel(..)
, BufferSerializeFormat(..)
, buffer_serialize_format_from_string, buffer_serialize_format_to_string
, pattern BUFFER_REPLACEMENT_CODEPOINT_DEFAULT

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
, buffer_get_glyph_flags
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

, Codepoint

, Direction(..)
, direction_to_string, direction_from_string
, direction_reverse, direction_is_valid
, direction_is_backward, direction_is_forward
, direction_is_vertical, direction_is_horizontal

, Feature(..)
, feature_to_string, feature_from_string

, Font
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

, GlyphInfo
, GlyphPosition(..)

, Key
, key_create
, key_create_n

, Language(..)
, language_from_string, language_to_string
, language_get_default

, MemoryMode(..)

, Position

, Script(..)
, script_from_iso15924_tag, script_to_iso15924_tag
, script_get_horizontal_direction
, script_from_string, script_to_string

, SegmentProperties(..)
-- , (==) provides hb_segment_properties_equal
-- , hash provides hb_segment_properties_hash


, shape -- the point of all of this
, shape_full
, shape_list_shapers

, ShapePlan
, shape_plan_create
, shape_plan_create_cached
, shape_plan_create2
, shape_plan_create_cached2
, shape_plan_execute
, shape_plan_get_shaper

, Shaper(SHAPER_INVALID)
, shaper_from_string, shaper_to_string

, Tag(Tag,TAG,TAG_NONE,TAG_MAX,TAG_MAX_SIGNED)
, tag_from_string, tag_to_string

, Variation(..)
, variation_from_string, variation_to_string
) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Coerce
import Data.Const
import Data.Functor
import Data.StateVar
import Data.String
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Traversable (for)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Const.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<stdlib.h>"
C.include "<hb.h>"
C.include "HsFFI.h"

key_create :: MonadIO m => m (Key a)
key_create = liftIO $ Key <$> mallocForeignPtrBytes 1

key_create_n :: MonadIO m => Int -> m (Int -> Key a)
key_create_n n = liftIO $ do
  fp <- mallocForeignPtrBytes n
  return $ \i ->
    if 0 < i && i < n
    then Key (plusForeignPtr fp i)
    else error "key_create_n: accessing an out of bound key"

blob_create :: MonadIO m => ByteString -> MemoryMode -> m Blob
blob_create bs mode = liftIO $ do
  (cstr, fromIntegral -> len) <- newByteStringCStringLen bs
  [C.block|hb_blob_t * {
    char * s = $(char * cstr);
    return hb_blob_create(s,$(unsigned int len),$(hb_memory_mode_t mode),s,free);
  }|] >>= foreignBlob

blob_create_from_file :: MonadIO m => FilePath -> m Blob
blob_create_from_file fp = liftIO $
  [C.exp|hb_blob_t * { hb_blob_create_from_file($str:fp) }|] >>= foreignBlob

blob_create_sub_blob :: MonadIO m => Blob -> Int -> Int -> m Blob
blob_create_sub_blob b (fromIntegral -> o) (fromIntegral -> l) = liftIO $
  [C.exp|hb_blob_t * { hb_blob_create_sub_blob($blob:b,$(int o),$(int l)) }|] >>= foreignBlob

blob_copy_writable_or_fail :: MonadIO m => Blob -> m (Maybe Blob)
blob_copy_writable_or_fail b = liftIO $
  [C.exp|hb_blob_t * { hb_blob_copy_writable_or_fail($blob:b) }|] >>= maybePeek foreignBlob

blob_get_length :: MonadIO m => Blob -> m Int
blob_get_length b = liftIO $ [C.exp|int { hb_blob_get_length($blob:b) }|] <&> fromIntegral

blob_is_immutable :: MonadIO m => Blob -> m Bool
blob_is_immutable b = liftIO $ [C.exp|hb_bool_t { hb_blob_is_immutable($blob:b) }|] <&> cbool

blob_make_immutable :: MonadIO m => Blob -> m ()
blob_make_immutable b = liftIO [C.block|void { hb_blob_make_immutable($blob:b); }|]

-- | hb_blob_get_data is unsafe under ForeignPtr management, this is safe
withBlobData :: Blob -> (ConstCStringLen -> IO r) -> IO r
withBlobData bfp k = withSelf bfp $ \bp -> alloca $ \ip -> do
  s <- [C.exp|const char * { hb_blob_get_data($(hb_blob_t * bp),$(unsigned int * ip)) }|]
  i <- peek ip
  k (constant s, fromIntegral i)

withBlobDataWritable :: Blob -> (CStringLen -> IO r) -> IO r
withBlobDataWritable bfp k = withSelf bfp $ \bp -> alloca $ \ip -> do
  s <- [C.exp|char * { hb_blob_get_data_writable($(hb_blob_t * bp),$(unsigned int * ip)) }|]
  i <- peek ip
  k (s, fromIntegral i)

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
buffer_get_glyph_flags :: MonadIO m => Buffer -> m [GlyphFlags]
buffer_get_glyph_flags b = liftIO $ alloca $ \plen -> do
  pinfos <- [C.exp|hb_glyph_info_t * { hb_buffer_get_glyph_infos($buffer:b,$(unsigned int * plen)) }|]
  len <- peek plen
  infos <- peekArray (fromIntegral len) pinfos
  for infos $ \info -> [C.exp|hb_glyph_flags_t { hb_glyph_info_get_glyph_flags($glyph-info:info) }|]

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

-- * directions

direction_reverse :: Direction -> Direction
direction_reverse d = [C.pure|hb_direction_t { HB_DIRECTION_REVERSE($(hb_direction_t d)) }|]

direction_is_backward :: Direction -> Bool
direction_is_backward d = cbool [C.pure|int { HB_DIRECTION_IS_BACKWARD($(hb_direction_t d)) }|]

direction_is_forward :: Direction -> Bool
direction_is_forward d = cbool [C.pure|int { HB_DIRECTION_IS_FORWARD($(hb_direction_t d)) }|]

direction_is_horizontal :: Direction -> Bool
direction_is_horizontal d = cbool [C.pure|int { HB_DIRECTION_IS_HORIZONTAL($(hb_direction_t d)) }|]

direction_is_vertical :: Direction -> Bool
direction_is_vertical d = cbool [C.pure|int { HB_DIRECTION_IS_VERTICAL($(hb_direction_t d)) }|]

direction_is_valid :: Direction -> Bool
direction_is_valid d = cbool [C.pure|int { HB_DIRECTION_IS_VALID($(hb_direction_t d)) }|]

-- * font faces

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
  g = alloca $ \(plen :: Ptr CUInt) -> do
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


-- * language

-- | The first time this is called it calls setLocale, which isn't thread safe.
-- For multithreaded use, first call once in an isolated fashion
language_get_default :: MonadIO m => m Language
language_get_default = liftIO $
  Language <$> [C.exp|hb_language_t { hb_language_get_default() }|]

-- * scripts

script_from_iso15924_tag :: Tag -> Script
script_from_iso15924_tag tag = [C.pure|hb_script_t { hb_script_from_iso15924_tag ($(hb_tag_t tag)) }|]

script_to_iso15924_tag :: Script -> Tag
script_to_iso15924_tag script = [C.pure|hb_tag_t { hb_script_to_iso15924_tag ($(hb_script_t script)) }|]

script_get_horizontal_direction :: Script -> Direction
script_get_horizontal_direction script = [C.pure|hb_direction_t { hb_script_get_horizontal_direction($(hb_script_t script)) }|]

script_from_string :: String -> Script
script_from_string = script_from_iso15924_tag . tag_from_string

script_to_string :: Script -> String
script_to_string = tag_to_string . script_to_iso15924_tag


shape :: MonadIO m => Font -> Buffer -> [Feature] -> m ()
shape font buffer features = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    [C.block|void{
      hb_shape($font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len));
    }|]

shape_full :: MonadIO m => Font -> Buffer -> [Feature] -> [Shaper] -> m ()
shape_full font buffer features shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.block|void{
         hb_shape_full($font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers));
      }|]

shape_plan_create :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Shaper] -> m ShapePlan
shape_plan_create face props features shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.exp|hb_shape_plan_t * {
         hb_shape_plan_create($face:face,$segment-properties:props,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers))
      }|] >>= foreignShapePlan

shape_plan_create_cached :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Shaper] -> m ShapePlan
shape_plan_create_cached face props features shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.exp|hb_shape_plan_t * {
         hb_shape_plan_create_cached($face:face,$segment-properties:props,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers))
      }|] >>= foreignShapePlan

shape_plan_create2 :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Int] -> [Shaper] -> m ShapePlan
shape_plan_create2 face props features coords shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArrayLen (fromIntegral <$> coords) $ \ (fromIntegral -> num_coords) pcoords ->
      withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
        [C.exp|hb_shape_plan_t * {
           hb_shape_plan_create2(
             $face:face,
             $segment-properties:props,
             $(const hb_feature_t * pfeatures),$(unsigned int len),
             $(const int * pcoords),$(unsigned int num_coords),
             $(const char * const * pshapers))
        }|] >>= foreignShapePlan

shape_plan_create_cached2 :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Int] -> [Shaper] -> m ShapePlan
shape_plan_create_cached2 face props features coords shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArrayLen (fromIntegral <$> coords) $ \ (fromIntegral -> num_coords) pcoords ->
      withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
        [C.exp|hb_shape_plan_t * {
           hb_shape_plan_create_cached2(
             $face:face,
             $segment-properties:props,
             $(const hb_feature_t * pfeatures),$(unsigned int len),
             $(const int * pcoords),$(unsigned int num_coords),
             $(const char * const * pshapers))
        }|] >>= foreignShapePlan

shape_plan_execute :: MonadIO m => ShapePlan -> Font -> Buffer -> [Feature] -> m Bool
shape_plan_execute plan font buffer features = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    [C.exp|hb_bool_t {
      hb_shape_plan_execute($shape-plan:plan,$font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len))
    }|] <&> cbool

shape_plan_get_shaper :: MonadIO m => ShapePlan -> m Shaper
shape_plan_get_shaper plan = liftIO $ [C.exp|const char * { hb_shape_plan_get_shaper($shape-plan:plan) }|] <&> Shaper

-- * 4 character tags

tag_from_string :: String -> Tag
tag_from_string = fromString

tag_to_string :: Tag -> String
tag_to_string t = unsafeLocalState $ allocaBytes 4 $ \buf -> do
  [C.exp|void { hb_tag_to_string($(hb_tag_t t),$(char * buf)) }|]
  peekCStringLen (buf,4)

