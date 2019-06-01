{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.Blob
( Blob
, blob_copy_writable_or_fail
, blob_create
, blob_create_from_file
, blob_create_sub_blob
, blob_get_length
, blob_is_immutable
, blob_make_immutable

, MemoryMode(..)

, with_blob_data
, with_blob_data_writable
) where

import Control.Monad.Primitive
import Data.ByteString (ByteString)
import Data.Const
import Data.Functor ((<&>))
import Foreign.C.Types
import Foreign.C.String
import Foreign.Const.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> C.bsCtx <> harfbuzzCtx
C.include "<stdlib.h>" -- free
C.include "<string.h>" -- strndup
C.include "<hb.h>"

blob_create :: PrimMonad m => ByteString -> MemoryMode -> m (Blob (PrimState m))
blob_create bs mode = unsafeIOToPrim $
  [C.block|hb_blob_t * {
    int len = $bs-len:bs;
    char * s = strndup($bs-ptr:bs,len);
    return hb_blob_create(s,len,$(hb_memory_mode_t mode),s,free);
  }|] >>= foreignBlob

blob_create_from_file :: (PrimMonad m, PrimState m ~ RealWorld) => FilePath -> m (Blob RealWorld)
blob_create_from_file fp = ioToPrim $
  [C.exp|hb_blob_t * { hb_blob_create_from_file($str:fp) }|] >>= foreignBlob

blob_create_sub_blob :: PrimMonad m => Blob (PrimState m) -> Int -> Int -> m (Blob (PrimState m))
blob_create_sub_blob b (fromIntegral -> o) (fromIntegral -> l) = unsafeIOToPrim $
  [C.exp|hb_blob_t * { hb_blob_create_sub_blob($blob:b,$(int o),$(int l)) }|] >>= foreignBlob

blob_copy_writable_or_fail :: PrimMonad m => Blob (PrimState m) -> m (Maybe (Blob (PrimState m)))
blob_copy_writable_or_fail b = unsafeIOToPrim $
  [C.exp|hb_blob_t * { hb_blob_copy_writable_or_fail($blob:b) }|] >>= maybePeek foreignBlob

blob_get_length :: PrimMonad m => Blob (PrimState m) -> m Int
blob_get_length b = unsafeIOToPrim $ [C.exp|int { hb_blob_get_length($blob:b) }|] <&> fromIntegral

blob_is_immutable :: PrimMonad m => Blob (PrimState m) -> m Bool
blob_is_immutable b = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_blob_is_immutable($blob:b) }|] <&> cbool

blob_make_immutable :: PrimMonad m => Blob (PrimState m) -> m ()
blob_make_immutable b = unsafeIOToPrim [C.block|void { hb_blob_make_immutable($blob:b); }|]

-- | hb_blob_get_data is unsafe under ForeignPtr management, this is safe
with_blob_data :: PrimBase m => Blob (PrimState m) -> (ConstCStringLen -> m r) -> m r
with_blob_data (Blob bfp) k = unsafeIOToPrim $ withForeignPtr bfp $ \bp -> alloca $ \ip -> do
  s <- [C.exp|const char * { hb_blob_get_data($(hb_blob_t * bp),$(unsigned int * ip)) }|]
  i <- peek ip
  unsafePrimToIO $ k (constant s, fromIntegral i)

with_blob_data_writable :: PrimBase m => Blob (PrimState m) -> (CStringLen -> m r) -> m r
with_blob_data_writable (Blob bfp) k = unsafeIOToPrim $ withForeignPtr bfp $ \bp -> alloca $ \ip -> do
  s <- [C.exp|char * { hb_blob_get_data_writable($(hb_blob_t * bp),$(unsigned int * ip)) }|]
  i <- peek ip
  unsafePrimToIO $ k (s, fromIntegral i)
