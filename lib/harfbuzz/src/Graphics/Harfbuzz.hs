{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language ForeignFunctionInterface #-}
module Graphics.Harfbuzz 
  ( Blob
  , blob_create
  , blob_create_from_file
  , blob_create_sub_blob
  , blob_copy_writable_or_fail
  , blob_get_length
  , blob_is_immutable
  , blob_make_immutable
  , withBlobData 

  , MemoryMode(..)

  -- * internals
  , foreignBlob
  , _hb_blob_destroy
  ) where

import Control.Monad.IO.Class
import Data.Const
import Data.Functor
import Data.ByteString as Strict
import Foreign.Const.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<stdlib.h>"
C.include "<hb.h>"

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

-- | hb_blob_get_data is unsafe under ForeignPtr management, this is safe
withBlobData :: Blob -> (ConstCStringLen -> IO r) -> IO r
withBlobData bfp k = withSelf bfp $ \bp -> alloca $ \ip -> do
  s <- [C.exp|const char * { hb_blob_get_data($(hb_blob_t * bp),$(unsigned int * ip)) }|]
  i <- peek ip
  k (constant s, fromIntegral i)

-- * Finalization

foreignBlob :: Ptr Blob -> IO Blob
foreignBlob = fmap Blob . newForeignPtr _hb_blob_destroy

foreign import ccall "hb.h &hb_blob_destroy" _hb_blob_destroy :: FinalizerPtr Blob
