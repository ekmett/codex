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

  , MemoryMode(..)

  -- hb_common.h
  , Tag(..)
  , tag_from_string, tag_to_string

  , Direction(..)
  , direction_to_string, direction_from_string

  , Script(..)
  , script_from_iso15924_tag, script_to_iso15924_tag
  , script_get_horizontal_direction
  , script_from_string, script_to_string

  , Language(..)
  , language_from_string, language_to_string
  , language_get_default

  -- hb_buffer.h
  , Buffer
  , buffer_create

  -- * internals
  , foreignBlob
  , _hb_blob_destroy
  ) where

import Control.Monad.IO.Class
import Data.Const
import Data.Functor
import Data.ByteString as Strict
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
blob_create_from_file fp = liftIO $ [C.exp|hb_blob_t * { hb_blob_create_from_file($str:fp) }|] >>= foreignBlob

blob_create_sub_blob :: MonadIO m => Blob -> Int -> Int -> m Blob

blob_create_sub_blob b (fromIntegral -> o) (fromIntegral -> l) = liftIO $ [C.exp|hb_blob_t * { hb_blob_create_sub_blob($blob:b,$(int o),$(int l)) }|] >>= foreignBlob

blob_copy_writable_or_fail :: MonadIO m => Blob -> m (Maybe Blob)
blob_copy_writable_or_fail b = liftIO $ [C.exp|hb_blob_t * { hb_blob_copy_writable_or_fail($blob:b) }|] >>= maybePeek foreignBlob

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

language_from_string :: String -> Language
language_from_string = fromString

language_to_string :: Language -> String
language_to_string l = unsafeLocalState (peekCString cstr) where
  cstr = [C.pure|const char * { hb_language_to_string($language:l) }|]

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
