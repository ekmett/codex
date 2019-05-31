{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.OpenType.Name
( Name(..)
, name_list_names
, name_get
) where

import Control.Monad.IO.Class
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

name_list_names :: MonadIO m => Face -> m [NameEntry]
name_list_names face = liftIO $
  alloca $ \plen -> do
    entries <- [C.exp|const hb_ot_name_entry_t * { hb_ot_name_list_names ($face:face,$(unsigned int * plen)) }|]
    len <- peek plen
    peekArray (fromIntegral len) entries -- do not free

name_get_ :: Face -> Name -> Language -> Int -> IO (Either Int String)
name_get_ face name language buflen =
  with (fromIntegral buflen) $ \pbuflen ->
    allocaBytes buflen $ \buf -> do
      full_len <- fromIntegral <$> [C.exp|unsigned int { hb_ot_name_get_utf32($face:face,$(hb_ot_name_id_t name),$language:language,$(unsigned int * pbuflen),$(uint32_t * buf))}|]
      if full_len > buflen
      then pure $ Left full_len
      else Right <$> do
        actual_len <- peek pbuflen
        peekArray (fromIntegral actual_len) (castPtr buf)

name_get :: MonadIO m => Face -> Name -> Language -> m (Maybe String)
name_get face name language = liftIO $
  name_get_ face name language 1024 >>= \case
    Left n -> name_get_ face name language n >>= \case -- slow path
      Left n' -> fail $ "ot_name_get: multiple fetches failed: actual length: " ++ show n'
      Right s -> pure $ Just s
    Right s -> pure $ Just s

