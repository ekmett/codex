{-# language LambdaCase #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language ForeignFunctionInterface #-}
module Graphics.Harfbuzz
  ( IsObject(..)
  -- hb_blob.h
  , Blob
  , blob_create
  , blob_create_from_file
  , blob_create_sub_blob
  , blob_copy_writable_or_fail
  , blob_get_length
  , blob_is_immutable
  , blob_make_immutable
  , withBlobData
  , withBlobDataWritable

  , Buffer
  , buffer_create
  , buffer_reset
  , buffer_clear_contents
  , buffer_pre_allocate
  , buffer_set_length
  , buffer_get_length
  , buffer_allocation_successful
  , buffer_reverse
  , buffer_reverse_range
  , buffer_reverse_clusters
  , buffer_add
  , buffer_add_string -- String
  , buffer_add_text -- Text
  , buffer_add_latin1 -- Char8.ByteString
  , buffer_add_utf8 -- UTF8 encoded ByteString
  , buffer_append
  , buffer_guess_segment_properties

  -- statevars
  , buffer_direction
  , buffer_script
  , buffer_language
  , buffer_flags
  , buffer_cluster_level
  , buffer_segment_properties
  , buffer_content_type
  , buffer_unicode_funcs

  , BufferFlags(..)
  , BufferContentType(..)
  , BufferClusterLevel(..)

  , Direction(..)
  , direction_to_string, direction_from_string
  , direction_reverse, direction_is_valid
  , direction_is_backward, direction_is_forward
  , direction_is_vertical, direction_is_horizontal

  , Feature(..)
  , feature_to_string, feature_from_string

  , Key(..) -- hb_user_data_key
  , key_create
  , key_create_n

  , Language(..)
  , language_from_string, language_to_string
  , language_get_default

  , MemoryMode(..)

  , Script(..)
  , script_from_iso15924_tag, script_to_iso15924_tag
  , script_get_horizontal_direction
  , script_from_string, script_to_string

  , SegmentProperties(..)

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

  -- * internals
  , foreignBlob
  , foreignBuffer
  , foreignUnicodeFuncs

  , _hb_blob_destroy
  , _hb_buffer_destroy
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

class IsObject t where
  reference :: MonadIO m => t -> m ()
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
  reference b = liftIO [C.block|void { hb_blob_reference($blob:b); }|]
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
  reference b = liftIO [C.block|void { hb_buffer_reference($buffer:b); }|]
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

buffer_add :: MonadIO m => Buffer -> Char -> Int -> m ()
buffer_add buffer codepoint (fromIntegral -> cluster) = liftIO
  [C.block|void { hb_buffer_add($buffer:buffer,$(hb_codepoint_t codepoint),$(unsigned int cluster)); }|]

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

buffer_guess_segment_properties :: MonadIO m => Buffer -> m ()
buffer_guess_segment_properties b = liftIO [C.block|void { hb_buffer_guess_segment_properties($buffer:b); }|]

-- * buffer statevars

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

-- * 4 character tags

tag_from_string :: String -> Tag
tag_from_string = fromString

tag_to_string :: Tag -> String
tag_to_string t = unsafeLocalState $ allocaBytes 4 $ \buf -> do
  [C.exp|void { hb_tag_to_string($(hb_tag_t t),$(char * buf)) }|]
  peekCStringLen (buf,4)

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

-- * language

-- | The first time this is called it calls setLocale, which isn't thread safe.

-- For multithreaded use, first call once in an isolated fashion
language_get_default :: MonadIO m => m Language
language_get_default = liftIO $
  Language <$> [C.exp|hb_language_t { hb_language_get_default() }|]

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
  [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_get_default() }|] >>= foreignUnicodeFuncs

unicode_funcs_get_parent :: MonadIO m => UnicodeFuncs -> m UnicodeFuncs
unicode_funcs_get_parent u = liftIO $
  [C.block|hb_unicode_funcs_t * {
    hb_unicode_funcs_t * p = hb_unicode_funcs_get_parent($unicode-funcs:u);
    hb_unicode_funcs_reference(p);
    return p;
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
unicode_combining_class uf codepoint = liftIO $ [C.exp|hb_unicode_combining_class_t { hb_unicode_combining_class($unicode-funcs:uf,$(hb_codepoint_t codepoint)) }|]

unicode_compose :: MonadIO m => UnicodeFuncs -> Char -> Char -> m (Maybe Char)
unicode_compose uf a b = liftIO $ alloca $ \c -> do
  ok <- [C.exp|hb_bool_t { hb_unicode_compose($unicode-funcs:uf,$(hb_codepoint_t a),$(hb_codepoint_t b),$(hb_codepoint_t * c)) }|]
  if cbool ok then Just <$> peek c else pure Nothing

unicode_decompose :: MonadIO m => UnicodeFuncs -> Char -> m (Maybe (Char, Char))
unicode_decompose uf a = liftIO $ allocaArray 2 $ \ pbc -> do
  ok <- [C.block|hb_bool_t {
    hb_codepoint_t * pbc = $(hb_codepoint_t * pbc);
    return hb_unicode_decompose($unicode-funcs:uf,$(hb_codepoint_t a),pbc,pbc+1);
  }|]
  if cbool ok then Just <$> ((,) <$> peek pbc <*> peek (advancePtr pbc 1)) else pure Nothing

unicode_general_category :: MonadIO m => UnicodeFuncs -> Char -> m UnicodeGeneralCategory
unicode_general_category uf codepoint = liftIO $ [C.exp|hb_unicode_general_category_t { hb_unicode_general_category($unicode-funcs:uf,$(hb_codepoint_t codepoint)) }|]

unicode_mirroring :: MonadIO m => UnicodeFuncs -> Char -> m Char
unicode_mirroring uf a = liftIO [C.exp|hb_codepoint_t { hb_unicode_mirroring($unicode-funcs:uf,$(hb_codepoint_t a)) }|]

unicode_script :: MonadIO m => UnicodeFuncs -> Char -> m Script
unicode_script uf a = liftIO [C.exp|hb_script_t { hb_unicode_script($unicode-funcs:uf,$(hb_codepoint_t a)) }|]

key_create :: MonadIO m => m (Key a)
key_create = liftIO $ Key <$> mallocForeignPtrBytes 1

key_create_n :: MonadIO m => Int -> m (Int -> Key a)
key_create_n n = liftIO $ do
  fp <- mallocForeignPtrBytes n
  return $ \i ->
    if 0 < i && i < n
    then Key (plusForeignPtr fp i)
    else error "key_create_n: accessing an out of bound key"

instance IsObject UnicodeFuncs where
  reference uf = liftIO [C.block|void { hb_unicode_funcs_reference($unicode-funcs:uf); }|]
  destroy uf = liftIO [C.block|void { hb_unicode_funcs_destroy($unicode-funcs:uf); }|]
  get_user_data b k = liftIO $ [C.exp|void * { hb_unicode_funcs_get_user_data($unicode-funcs:b,$key:k) }|] <&> castPtr
  set_user_data b k (castPtr -> v) (castFunPtr -> d) (boolc -> replace) = liftIO $
    [C.exp|hb_bool_t{ hb_unicode_funcs_set_user_data($unicode-funcs:b,$key:k,$(void * v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|] <&> cbool

-- * Finalization

foreignBlob :: Ptr Blob -> IO Blob
foreignBlob = fmap Blob . newForeignPtr _hb_blob_destroy

foreignBuffer :: Ptr Buffer -> IO Buffer
foreignBuffer = fmap Buffer . newForeignPtr _hb_buffer_destroy

foreignUnicodeFuncs :: Ptr UnicodeFuncs -> IO UnicodeFuncs
foreignUnicodeFuncs = fmap UnicodeFuncs . newForeignPtr _hb_unicode_funcs_destroy

foreign import ccall "hb.h &hb_blob_destroy" _hb_blob_destroy :: FinalizerPtr Blob
foreign import ccall "hb.h &hb_buffer_destroy" _hb_buffer_destroy :: FinalizerPtr Buffer
foreign import ccall "hb.h &hb_unicode_funcs_destroy" _hb_unicode_funcs_destroy :: FinalizerPtr UnicodeFuncs
