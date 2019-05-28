{-# language LambdaCase #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ForeignFunctionInterface #-}
module Graphics.Harfbuzz
  ( IsObject(..)
  -- hb_blob.h
  , Blob
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
  , buffer_add
  , buffer_add_char
  , buffer_add_latin1 -- Char8.ByteString
  , buffer_add_string -- String
  , buffer_add_text -- Text
  , buffer_add_utf8 -- UTF8 encoded ByteString
  , buffer_allocation_successful
  , buffer_append
  , buffer_clear_contents
  , buffer_create
  , buffer_get_length
  , buffer_get_glyph_positions
  , buffer_guess_segment_properties
  , buffer_normalize_glyphs
  , buffer_pre_allocate
  , buffer_reset
  , buffer_reverse
  , buffer_reverse_clusters
  , buffer_reverse_range
  , buffer_set_length
  -- buffer_get_glyph_infos
  -- buffer_serialize_glyphs
  -- buffer_deserialize_glyphs
  -- buffer_serialize_format_from_string
  -- buffer_serialize_format_to_string
  -- buffer_serialize_list_formats
  -- buffer_diff
  -- buffer_set_message_func
  -- glyph_info_get_glyph_flags

  -- statevars
  , buffer_cluster_level
  , buffer_content_type
  , buffer_direction
  , buffer_flags
  , buffer_invisible_glyph
  , buffer_language
  , buffer_replacement_codepoint
  , buffer_script
  , buffer_segment_properties
  , buffer_unicode_funcs

  , BufferFlags(..)
  , BufferContentType(..)
  , BufferClusterLevel(..)
  , BufferSerializeFormat(..)
  , pattern BUFFER_REPLACEMENT_CODEPOINT_DEFAULT

  , Codepoint

  , Direction(..)
  , direction_to_string, direction_from_string
  , direction_reverse, direction_is_valid
  , direction_is_backward, direction_is_forward
  , direction_is_vertical, direction_is_horizontal

  , Face(..)
  , face_collect_unicodes
  , face_collect_variation_selectors
  , face_collect_variation_unicodes
  , face_count
  , face_create
  , face_create_for_tables
  , face_glyph_count -- statevar
  , face_index -- statevar
  , face_is_immutable
  , face_make_immutable
  , face_builder_create
  , face_builder_add_table
  , face_reference_blob
  , face_reference_table
  , face_upem -- statevar

  , Feature(..)
  , feature_to_string, feature_from_string

  , GlyphPosition(..)

  , Key(..) -- hb_user_data_key
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
  -- segment_properties_equal
  -- segment_properties_hash

  , Set(..)
  , set_add
  , set_add_range
  , set_allocation_successful
  , set_clear
  , set_create
  , set_del
  , set_del_range
  , set_get_max
  , set_get_min
  , set_get_population
  , set_has
  , set_intersect
  , set_is_empty
  , set_is_equal
  , set_is_subset
  , set_next
  , set_next_range
  , set_previous
  , set_previous_range
  , set_set
  , set_subtract
  , set_symmetric_difference
  , set_union
  , pattern SET_VALUE_INVALID

  , Tag(..)
  , tag_from_string, tag_to_string

  , UnicodeCombiningClass(..)
  , UnicodeFuncs(..)
  , UnicodeGeneralCategory(..)
  , unicode_funcs_create
  , unicode_funcs_get_default
  , unicode_funcs_get_parent
  , unicode_funcs_is_immutable
  , unicode_funcs_make_immutable
  , unicode_funcs_set_combining_class_func
  , unicode_funcs_set_compose_func
  , unicode_funcs_set_decompose_func
  , unicode_funcs_set_general_category_func
  , unicode_funcs_set_mirroring_func
  , unicode_funcs_set_script_func

  , unicode_combining_class
  , unicode_compose
  , unicode_decompose
  , unicode_general_category
  , unicode_mirroring
  , unicode_script

  , Variation(..)
  , variation_from_string, variation_to_string

  , version
  , version_string
  , pattern VERSION_MAJOR
  , pattern VERSION_MINOR
  , pattern VERSION_MICRO

  -- * internals
  , foreignBlob
  , foreignBuffer
  , foreignFace
  , foreignSet
  , foreignUnicodeFuncs

  , _hb_blob_destroy
  , _hb_buffer_destroy
  , _hb_set_destroy
  , _hb_face_destroy
  , _hb_unicode_funcs_destroy
  ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Const
import Data.Functor
import Data.StateVar
import Data.String
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Version (Version, makeVersion)
import Foreign.C.String
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

class IsObject t where
  reference :: MonadIO m => t -> m (Ptr t)
  destroy :: MonadIO m => t -> m ()
  set_user_data :: MonadIO m => t -> Key a -> Ptr a -> FinalizerPtr a -> Bool -> m Bool
  get_user_data :: MonadIO m => t -> Key a -> m (Ptr a)

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

instance IsObject Blob where
  reference b = liftIO [C.exp|hb_blob_t * { hb_blob_reference($blob:b) }|]
  destroy b = liftIO [C.block|void { hb_blob_destroy($blob:b); }|]
  get_user_data b k = liftIO $ [C.exp|void * { hb_blob_get_user_data($blob:b,$key:k) }|] <&> castPtr
  set_user_data b k (castPtr -> v) (castFunPtr -> d) (boolc -> replace) = liftIO $
    [C.exp|hb_bool_t { hb_blob_set_user_data($blob:b,$key:k,$(void * v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|] <&> cbool

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

instance IsObject Buffer where
  reference b = liftIO [C.exp|hb_buffer_t * { hb_buffer_reference($buffer:b) }|]
  destroy b = liftIO [C.block|void { hb_buffer_destroy($buffer:b); }|]
  get_user_data b k = liftIO $ [C.exp|void * { hb_buffer_get_user_data($buffer:b,$key:k) }|] <&> castPtr
  set_user_data b k (castPtr -> v) (castFunPtr -> d) (boolc -> replace) = liftIO $
    [C.exp|hb_bool_t { hb_buffer_set_user_data($buffer:b,$key:k,$(void * v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|] <&> cbool

buffer_create :: MonadIO m => m Buffer
buffer_create = liftIO $ [C.exp|hb_buffer_t * { hb_buffer_create() }|] >>= foreignBuffer

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

buffer_guess_segment_properties :: MonadIO m => Buffer -> m ()
buffer_guess_segment_properties b = liftIO [C.block|void { hb_buffer_guess_segment_properties($buffer:b); }|]

buffer_normalize_glyphs :: MonadIO m => Buffer -> m ()
buffer_normalize_glyphs b = liftIO [C.block|void { hb_buffer_normalize_glyphs($buffer:b); }|]

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

instance IsObject Face where
  reference b = liftIO [C.exp|hb_face_t * { hb_face_reference($face:b) }|]
  destroy b = liftIO [C.block|void { hb_face_destroy($face:b); }|]
  get_user_data b k = liftIO $ [C.exp|void * { hb_face_get_user_data($face:b,$key:k) }|] <&> castPtr
  set_user_data b k (castPtr -> v) (castFunPtr -> d) (boolc -> replace) = liftIO $
    [C.exp|hb_bool_t { hb_face_set_user_data($face:b,$key:k,$(void * v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|] <&> cbool

face_builder_create :: MonadIO m => m Face
face_builder_create = liftIO $ [C.exp|hb_face_t * { hb_face_builder_create() }|] >>= foreignFace

face_builder_add_table :: MonadIO m => Face -> Tag -> Blob -> m Bool
face_builder_add_table f t b = liftIO $ [C.exp|hb_bool_t { hb_face_builder_add_table($face:f,$(hb_tag_t t),$blob:b) }|] <&> cbool

face_reference_blob :: MonadIO m => Face -> m Blob
face_reference_blob f = liftIO $ [C.exp|hb_blob_t * { hb_face_reference_blob($face:f) }|] >>= foreignBlob

face_reference_table :: MonadIO m => Face -> Tag -> m Blob
face_reference_table f t = liftIO $ [C.exp|hb_blob_t * { hb_face_reference_table($face:f,$(hb_tag_t t)) }|] >>= foreignBlob

-- add the unicode codepoints present in the face to the given set
face_collect_unicodes :: MonadIO m => Face -> Set -> m ()
face_collect_unicodes f s = liftIO [C.block|void { hb_face_collect_unicodes($face:f,$set:s); }|]

face_collect_variation_selectors :: MonadIO m => Face -> Set -> m ()
face_collect_variation_selectors f s = liftIO [C.block|void { hb_face_collect_variation_selectors($face:f,$set:s); }|]

face_collect_variation_unicodes :: MonadIO m => Face -> Codepoint -> Set -> m ()
face_collect_variation_unicodes f variation s = liftIO [C.block|void { hb_face_collect_variation_unicodes($face:f,$(hb_codepoint_t variation),$set:s); }|]

face_count :: MonadIO m => Blob -> m Int
face_count b = liftIO $ [C.exp|int { hb_face_count($blob:b) }|] <&> fromIntegral

face_create :: MonadIO m => Blob -> Int -> m Face
face_create b (fromIntegral -> i) = liftIO $
  [C.exp|hb_face_t * { hb_face_create($blob:b,$(int i)) }|] >>= foreignFace

face_create_for_tables :: MonadIO m => (Face -> Tag -> IO Blob) -> m Face
face_create_for_tables fun = liftIO $ do
  (castFunPtr -> f) <- mkReferenceTableFunc $ \ pface tag _ -> do
    face <- [C.exp|hb_face_t * { hb_face_reference($(hb_face_t * pface)) }|] >>= foreignFace
    b <- fun face tag
    reference b
  [C.block|hb_face_t * {
    hb_reference_table_func_t f = $(hb_reference_table_func_t f);
    return hb_face_create_for_tables(f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|] >>= foreignFace

-- | Subsumes @hb_face_get_glyph_count@ and @hb_face_set_glyph_count@
face_glyph_count :: Face -> StateVar Int
face_glyph_count f = StateVar g s where
  g = [C.exp|int { hb_face_get_glyph_count($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.block|void { hb_face_set_glyph_count($face:f,$(unsigned int v)); }|]

-- | Subsumes @hb_face_get_index@ ancd @hb_face_set_index@
face_index :: Face -> StateVar Int
face_index f = StateVar g s where
  g = [C.exp|int { hb_face_get_index($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.block|void { hb_face_set_index($face:f,$(unsigned int v)); }|]

face_upem :: Face -> StateVar Int
face_upem f = StateVar g s where
  g = [C.exp|int { hb_face_get_upem($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.block|void { hb_face_set_upem($face:f,$(unsigned int v)); }|]

face_is_immutable :: MonadIO m => Face -> m Bool
face_is_immutable b = liftIO $ [C.exp|hb_bool_t { hb_face_is_immutable($face:b) }|] <&> cbool

face_make_immutable :: MonadIO m => Face -> m ()
face_make_immutable b = liftIO [C.block|void { hb_face_make_immutable($face:b); }|]

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

-- * sets
--
instance IsObject Set where
  reference b = liftIO [C.exp|hb_set_t * { hb_set_reference($set:b) }|]
  destroy b = liftIO [C.block|void { hb_set_destroy($set:b); }|]
  get_user_data b k = liftIO $ [C.exp|void * { hb_set_get_user_data($set:b,$key:k) }|] <&> castPtr
  set_user_data b k (castPtr -> v) (castFunPtr -> d) (boolc -> replace) = liftIO $
    [C.exp|hb_bool_t { hb_set_set_user_data($set:b,$key:k,$(void * v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|] <&> cbool

set_add :: MonadIO m => Set -> Codepoint -> m ()
set_add s c = liftIO $ [C.block|void { hb_set_add($set:s,$(hb_codepoint_t c)); }|]

set_add_range :: MonadIO m => Set -> Codepoint -> Codepoint -> m ()
set_add_range s lo hi = liftIO $ [C.block|void { hb_set_add_range($set:s,$(hb_codepoint_t lo),$(hb_codepoint_t hi)); }|]

set_allocation_successful :: MonadIO m => Set -> m Bool
set_allocation_successful s = liftIO $ [C.exp|hb_bool_t { hb_set_allocation_successful($set:s) }|] <&> cbool

set_clear :: MonadIO m => Set -> m ()
set_clear s = liftIO $ [C.block|void { hb_set_clear($set:s); }|]

set_create :: MonadIO m => m Set
set_create = liftIO $ [C.exp|hb_set_t * { hb_set_create() }|] >>= foreignSet

set_del :: MonadIO m => Set -> Codepoint -> m ()
set_del s c = liftIO $ [C.block|void { hb_set_del($set:s,$(hb_codepoint_t c)); }|]

set_del_range :: MonadIO m => Set -> Codepoint -> Codepoint -> m ()
set_del_range s lo hi = liftIO $ [C.block|void { hb_set_del_range($set:s,$(hb_codepoint_t lo),$(hb_codepoint_t hi)); }|]

set_get_max :: MonadIO m => Set -> m Codepoint
set_get_max s = liftIO [C.exp|hb_codepoint_t { hb_set_get_max($set:s) }|]

set_get_min :: MonadIO m => Set -> m Codepoint
set_get_min s = liftIO [C.exp|hb_codepoint_t { hb_set_get_min($set:s) }|]

set_get_population :: MonadIO m => Set -> m Int
set_get_population s = liftIO $ [C.exp|unsigned int { hb_set_get_population($set:s) }|] <&> fromIntegral

set_has :: MonadIO m => Set -> Codepoint -> m Bool
set_has s c = liftIO $ [C.exp|hb_bool_t { hb_set_has($set:s,$(hb_codepoint_t c)) }|] <&> cbool

set_intersect :: MonadIO m => Set -> Set -> m ()
set_intersect s other = liftIO [C.block|void { hb_set_intersect($set:s,$set:other); }|]

set_is_empty :: MonadIO m => Set -> m Bool
set_is_empty s = liftIO $ [C.exp|hb_bool_t { hb_set_is_empty($set:s) }|] <&> cbool

set_is_equal :: MonadIO m => Set -> Set -> m Bool
set_is_equal s t = liftIO $ [C.exp|hb_bool_t { hb_set_is_equal($set:s,$set:t) }|] <&> cbool

set_is_subset :: MonadIO m => Set -> Set -> m Bool
set_is_subset s t = liftIO $ [C.exp|hb_bool_t { hb_set_is_subset($set:s,$set:t) }|] <&> cbool

-- | Start with SET_VALUE_INVALID
set_next :: MonadIO m => Set -> Codepoint -> m (Maybe Codepoint)
set_next s c = liftIO $ with c $ \p -> do
  b <- [C.exp|hb_bool_t { hb_set_next($set:s,$(hb_codepoint_t * p)) }|]
  if cbool b then Just <$> peek p else pure Nothing

-- | Start with SET_VALUE_INVALID
set_next_range :: MonadIO m => Set -> Codepoint -> m (Maybe (Codepoint, Codepoint))
set_next_range s c = liftIO $ allocaArray 2 $ \p -> do
  let q = advancePtr p 1
  poke q c
  b <- [C.exp|hb_bool_t { hb_set_next_range($set:s,$(hb_codepoint_t * p),$(hb_codepoint_t * q)) }|]
  if cbool b
  then do
    lo <- peek p
    hi <- peek q
    pure $ Just (lo,hi)
  else pure Nothing

-- | Start with SET_VALUE_INVALID
set_previous :: MonadIO m => Set -> Codepoint -> m (Maybe Codepoint)
set_previous s c = liftIO $ with c $ \p -> do
  b <- [C.exp|hb_bool_t { hb_set_previous($set:s,$(hb_codepoint_t * p)) }|]
  if cbool b then Just <$> peek p else pure Nothing

-- | Start with SET_VALUE_INVALID
set_previous_range :: MonadIO m => Set -> Codepoint -> m (Maybe (Codepoint, Codepoint))
set_previous_range s c = liftIO $ allocaArray 2 $ \p -> do
  let q = advancePtr p 1
  poke p c
  b <- [C.exp|hb_bool_t { hb_set_previous_range($set:s,$(hb_codepoint_t * p),$(hb_codepoint_t * q)) }|]
  if cbool b
  then do
    lo <- peek p
    hi <- peek q
    pure $ Just (lo,hi)
  else pure Nothing

set_set :: MonadIO m => Set -> Set -> m ()
set_set s t = liftIO [C.block|void { hb_set_set($set:s,$set:t); }|]

set_subtract :: MonadIO m => Set -> Set -> m ()
set_subtract s other = liftIO [C.block|void { hb_set_subtract($set:s,$set:other); }|]

set_symmetric_difference :: MonadIO m => Set -> Set -> m ()
set_symmetric_difference s other = liftIO [C.block|void { hb_set_symmetric_difference($set:s,$set:other); }|]

set_union :: MonadIO m => Set -> Set -> m ()
set_union s other = liftIO [C.block|void { hb_set_union($set:s,$set:other); }|]

-- * 4 character tags

tag_from_string :: String -> Tag
tag_from_string = fromString

tag_to_string :: Tag -> String
tag_to_string t = unsafeLocalState $ allocaBytes 4 $ \buf -> do
  [C.exp|void { hb_tag_to_string($(hb_tag_t t),$(char * buf)) }|]
  peekCStringLen (buf,4)

-- * unicode functions

unicode_funcs_create :: MonadIO m => UnicodeFuncs -> m UnicodeFuncs
unicode_funcs_create parent = liftIO $
  [C.block|hb_unicode_funcs_t * {
    hb_unicode_funcs_t * p = $unicode-funcs:parent;
    hb_unicode_funcs_reference(p);
    return hb_unicode_funcs_create(p);
  }|] >>= foreignUnicodeFuncs

unicode_funcs_get_default :: MonadIO m => m UnicodeFuncs
unicode_funcs_get_default = liftIO $
  [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_reference(hb_unicode_funcs_get_default()) }|] >>= foreignUnicodeFuncs

unicode_funcs_get_parent :: MonadIO m => UnicodeFuncs -> m UnicodeFuncs
unicode_funcs_get_parent u = liftIO $
  [C.block|hb_unicode_funcs_t * {
    hb_unicode_funcs_t * p = hb_unicode_funcs_get_parent($unicode-funcs:u);
    return hb_unicode_funcs_reference(p);
  }|] >>= foreignUnicodeFuncs

unicode_funcs_is_immutable :: MonadIO m => UnicodeFuncs -> m Bool
unicode_funcs_is_immutable b = liftIO $ [C.exp|hb_bool_t { hb_unicode_funcs_is_immutable($unicode-funcs:b) }|] <&> cbool

unicode_funcs_make_immutable :: MonadIO m => UnicodeFuncs -> m ()
unicode_funcs_make_immutable b = liftIO [C.block|void { hb_unicode_funcs_make_immutable($unicode-funcs:b); }|]

foreign import ccall "wrapper" mkUnicodeCombiningClassFunc :: UnicodeCombiningClassFunc a -> IO (FunPtr (UnicodeCombiningClassFunc a))
foreign import ccall "wrapper" mkUnicodeComposeFunc :: UnicodeComposeFunc a -> IO (FunPtr (UnicodeComposeFunc a))
foreign import ccall "wrapper" mkUnicodeDecomposeFunc :: UnicodeDecomposeFunc a -> IO (FunPtr (UnicodeDecomposeFunc a))
foreign import ccall "wrapper" mkUnicodeGeneralCategoryFunc :: UnicodeGeneralCategoryFunc a -> IO (FunPtr (UnicodeGeneralCategoryFunc a))
foreign import ccall "wrapper" mkUnicodeMirroringFunc :: UnicodeMirroringFunc a -> IO (FunPtr (UnicodeMirroringFunc a))
foreign import ccall "wrapper" mkUnicodeScriptFunc :: UnicodeScriptFunc a -> IO (FunPtr (UnicodeScriptFunc a))
foreign import ccall "wrapper" mkReferenceTableFunc :: ReferenceTableFunc a -> IO (FunPtr (ReferenceTableFunc a))

unicode_funcs_set_combining_class_func :: MonadIO m => UnicodeFuncs -> (Char -> IO UnicodeCombiningClass) -> m ()
unicode_funcs_set_combining_class_func uf fun = liftIO $ do
  (castFunPtr -> f) <- mkUnicodeCombiningClassFunc $ \ _ c _ -> fun c
  [C.block|void {
    hb_unicode_combining_class_func_t f = $(hb_unicode_combining_class_func_t f);
    hb_unicode_funcs_set_combining_class_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_compose_func :: MonadIO m => UnicodeFuncs -> (Char -> Char -> IO (Maybe Char)) -> m ()
unicode_funcs_set_compose_func uf fun = liftIO $ do
  (castFunPtr -> f) <- mkUnicodeComposeFunc $ \ _ a b c _ -> fun a b >>= \case
     Nothing -> pure $ boolc False
     Just ab -> boolc True <$ poke c ab
  [C.block|void {
    hb_unicode_compose_func_t f = $(hb_unicode_compose_func_t f);
    hb_unicode_funcs_set_compose_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_decompose_func :: MonadIO m => UnicodeFuncs -> (Char -> IO (Maybe (Char,Char))) -> m ()
unicode_funcs_set_decompose_func uf fun = liftIO $ do
  (castFunPtr -> f) <- mkUnicodeDecomposeFunc $ \ _ a pb pc _ -> fun a >>= \case
     Nothing -> pure $ boolc False
     Just (b,c) -> boolc True <$ (poke pb b *> poke pc c)
  [C.block|void {
    hb_unicode_decompose_func_t f = $(hb_unicode_decompose_func_t f);
    hb_unicode_funcs_set_decompose_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_general_category_func :: MonadIO m => UnicodeFuncs -> (Char -> IO UnicodeGeneralCategory) -> m ()
unicode_funcs_set_general_category_func uf fun = liftIO $ do
  (castFunPtr -> f) <- mkUnicodeGeneralCategoryFunc $ \ _ c _ -> fun c
  [C.block|void {
    hb_unicode_general_category_func_t f = $(hb_unicode_general_category_func_t f);
    hb_unicode_funcs_set_general_category_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_mirroring_func :: MonadIO m => UnicodeFuncs -> (Char -> IO Char) -> m ()
unicode_funcs_set_mirroring_func uf fun = liftIO $ do
  (castFunPtr -> f) <- mkUnicodeMirroringFunc $ \ _ a _ -> fun a
  [C.block|void {
    hb_unicode_mirroring_func_t f = $(hb_unicode_mirroring_func_t f);
    hb_unicode_funcs_set_mirroring_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_script_func :: MonadIO m => UnicodeFuncs -> (Char -> IO Script) -> m ()
unicode_funcs_set_script_func uf fun = liftIO $ do
  (castFunPtr -> f) <- mkUnicodeScriptFunc $ \ _ a _ -> fun a
  [C.block|void {
    hb_unicode_script_func_t f = $(hb_unicode_script_func_t f);
    hb_unicode_funcs_set_script_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_combining_class :: MonadIO m => UnicodeFuncs -> Char -> m UnicodeCombiningClass
unicode_combining_class uf (c2w -> codepoint) = liftIO $ [C.exp|hb_unicode_combining_class_t { hb_unicode_combining_class($unicode-funcs:uf,$(hb_codepoint_t codepoint)) }|]

unicode_compose :: MonadIO m => UnicodeFuncs -> Char -> Char -> m (Maybe Char)
unicode_compose uf (c2w -> a) (c2w -> b) = liftIO $ alloca $ \c -> do
  ok <- [C.exp|hb_bool_t { hb_unicode_compose($unicode-funcs:uf,$(hb_codepoint_t a),$(hb_codepoint_t b),$(hb_codepoint_t * c)) }|]
  if cbool ok then Just . w2c <$> peek c else pure Nothing

unicode_decompose :: MonadIO m => UnicodeFuncs -> Char -> m (Maybe (Char, Char))
unicode_decompose uf (c2w -> a) = liftIO $ allocaArray 2 $ \ pbc -> do
  ok <- [C.block|hb_bool_t {
    hb_codepoint_t * pbc = $(hb_codepoint_t * pbc);
    return hb_unicode_decompose($unicode-funcs:uf,$(hb_codepoint_t a),pbc,pbc+1);
  }|]
  if cbool ok then do
    b <- peek pbc
    c <- peek (advancePtr pbc 1)
    pure $ Just (w2c b, w2c c)
  else pure Nothing

unicode_general_category :: MonadIO m => UnicodeFuncs -> Char -> m UnicodeGeneralCategory
unicode_general_category uf (c2w -> codepoint) = liftIO $ [C.exp|hb_unicode_general_category_t { hb_unicode_general_category($unicode-funcs:uf,$(hb_codepoint_t codepoint)) }|]

unicode_mirroring :: MonadIO m => UnicodeFuncs -> Char -> m Char
unicode_mirroring uf (c2w -> a) = liftIO $ [C.exp|hb_codepoint_t { hb_unicode_mirroring($unicode-funcs:uf,$(hb_codepoint_t a)) }|] <&> w2c

unicode_script :: MonadIO m => UnicodeFuncs -> Char -> m Script
unicode_script uf (c2w -> a) = liftIO [C.exp|hb_script_t { hb_unicode_script($unicode-funcs:uf,$(hb_codepoint_t a)) }|]

instance IsObject UnicodeFuncs where
  reference uf = liftIO [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_reference($unicode-funcs:uf) }|]
  destroy uf = liftIO [C.block|void { hb_unicode_funcs_destroy($unicode-funcs:uf); }|]
  get_user_data b k = liftIO $ [C.exp|void * { hb_unicode_funcs_get_user_data($unicode-funcs:b,$key:k) }|] <&> castPtr
  set_user_data b k (castPtr -> v) (castFunPtr -> d) (boolc -> replace) = liftIO $
    [C.exp|hb_bool_t{ hb_unicode_funcs_set_user_data($unicode-funcs:b,$key:k,$(void * v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|] <&> cbool

version :: MonadIO m => m Version
version = liftIO $ allocaArray 3 $ \abc -> do
  [C.block|void {
     unsigned int * abc = $(unsigned int * abc);
     hb_version(abc,abc+1,abc+2);
  }|]
  a <- peek abc
  b <- peek (advancePtr abc 1)
  c <- peek (advancePtr abc 2)
  pure $ makeVersion [fromIntegral a,fromIntegral b,fromIntegral c]

version_string :: MonadIO m => m String
version_string = liftIO $ [C.exp|const char * { hb_version_string() }|] >>= peekCString

-- * Finalization

foreignBlob :: Ptr Blob -> IO Blob
foreignBlob = fmap Blob . newForeignPtr _hb_blob_destroy

foreignBuffer :: Ptr Buffer -> IO Buffer
foreignBuffer = fmap Buffer . newForeignPtr _hb_buffer_destroy

foreignFace :: Ptr Face -> IO Face
foreignFace = fmap Face . newForeignPtr _hb_face_destroy

foreignSet :: Ptr Set -> IO Set
foreignSet = fmap Set . newForeignPtr _hb_set_destroy

foreignUnicodeFuncs :: Ptr UnicodeFuncs -> IO UnicodeFuncs
foreignUnicodeFuncs = fmap UnicodeFuncs . newForeignPtr _hb_unicode_funcs_destroy

foreign import ccall "hb.h &hb_blob_destroy"          _hb_blob_destroy          :: FinalizerPtr Blob
foreign import ccall "hb.h &hb_buffer_destroy"        _hb_buffer_destroy        :: FinalizerPtr Buffer
foreign import ccall "hb.h &hb_face_destroy"          _hb_face_destroy          :: FinalizerPtr Face
foreign import ccall "hb.h &hb_set_destroy"           _hb_set_destroy           :: FinalizerPtr Set
foreign import ccall "hb.h &hb_unicode_funcs_destroy" _hb_unicode_funcs_destroy :: FinalizerPtr UnicodeFuncs
