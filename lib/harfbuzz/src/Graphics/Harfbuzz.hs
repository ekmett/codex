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
  -- blob_set_user_data

  , Buffer
  , buffer_create
  , buffer_reset
  , buffer_clear_contents
  , buffer_pre_allocate
  , buffer_set_length
  , buffer_get_length
  , buffer_allocation_successful
  , buffer_add
  -- statevars
  , buffer_direction
  , buffer_script
  , buffer_language
  , buffer_flags
  , buffer_cluster_level
  , buffer_segment_properties

  , BufferFlags(..)
  , BufferClusterLevel(..)

  , Direction(..)
  , direction_to_string, direction_from_string
  , direction_reverse, direction_is_valid
  , direction_is_backward, direction_is_forward
  , direction_is_vertical, direction_is_horizontal

  , Feature(..)
  , feature_to_string, feature_from_string

  , Language(..)
  , language_from_string, language_to_string
  , language_get_default

  , MemoryMode(..)

  , Script(..)
  , script_from_iso15924_tag, script_to_iso15924_tag
  , script_get_horizontal_direction
  , script_from_string, script_to_string

  , Tag(..)
  , tag_from_string, tag_to_string

  , Variation(..)
  , variation_from_string, variation_to_string

  -- * internals
  , foreignBlob
  , _hb_blob_destroy
  ) where

import Control.Monad.IO.Class
import Data.Const
import Data.Functor
import Data.ByteString as Strict
import Data.StateVar
import Data.String
import Foreign.C.String
import Foreign.Const.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<stdlib.h>"
C.include "<hb.h>"

class IsObject t where
  reference :: MonadIO m => t -> m ()
  destroy :: MonadIO m => t -> m ()

blob_create :: MonadIO m => Strict.ByteString -> MemoryMode -> m Blob
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

buffer_add :: MonadIO m => Buffer -> Char -> Int -> m ()
buffer_add buffer codepoint (fromIntegral -> cluster) = liftIO
  [C.block|void { hb_buffer_add($buffer:buffer,$(hb_codepoint_t codepoint),$(unsigned int cluster)); }|]

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

-- | Subsumes @hb_buffer_get_segment_properties@ and @hb_buffer_set_segment_properties@
buffer_segment_properties :: Buffer -> StateVar SegmentProperties
buffer_segment_properties b = StateVar g s where
  g = alloca $ \props -> [C.block|void { hb_buffer_get_segment_properties($buffer:b,$(hb_segment_properties_t * props)); }|] *> peek props
  s v = with v $ \props -> [C.block|void { hb_buffer_set_segment_properties($buffer:b,$(const hb_segment_properties_t * props)); }|]

instance IsObject Buffer where
  reference b = liftIO [C.block|void { hb_buffer_reference($buffer:b); }|]
  destroy b = liftIO [C.block|void { hb_buffer_destroy($buffer:b); }|]

-- * 4 character tags

tag_from_string :: String -> Tag
tag_from_string = fromString

tag_to_string :: Tag -> String
tag_to_string t = unsafeLocalState $ allocaBytes 4 $ \buf -> do
  [C.exp|void { hb_tag_to_string($(hb_tag_t t),$(char * buf)) }|]
  peekCStringLen (buf,4)

-- * directions

direction_from_string :: String -> Direction
direction_from_string = fromString

direction_to_string :: Direction -> String
direction_to_string t = unsafeLocalState $
  [C.exp|const char * { hb_direction_to_string($(hb_direction_t t)) }|] >>= peekCString

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
--
-- For multithreaded use, first call once in an isolated fashion
language_get_default :: MonadIO m => m Language
language_get_default = liftIO $ Language <$> [C.exp|hb_language_t { hb_language_get_default() }|]

-- * Finalization

foreignBlob :: Ptr Blob -> IO Blob
foreignBlob = fmap Blob . newForeignPtr _hb_blob_destroy

foreignBuffer :: Ptr Buffer -> IO Buffer
foreignBuffer = fmap Buffer . newForeignPtr _hb_buffer_destroy

foreign import ccall "hb.h &hb_blob_destroy" _hb_blob_destroy :: FinalizerPtr Blob

foreign import ccall "hb.h &hb_buffer_destroy" _hb_buffer_destroy :: FinalizerPtr Buffer
