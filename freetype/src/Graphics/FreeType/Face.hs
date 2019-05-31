{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.FreeType.Face
( Face
-- * create a reference counted library with all of the usual defaults
, new_face

-- * reference counting
, reference_face
, done_face
) where

import Control.Monad.IO.Class
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Language.C.Inline as C
import Graphics.FreeType.Internal

C.context $ C.baseCtx <> C.bsCtx <> C.fptrCtx <> freeTypeCtx
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"
C.include "ft.h"

-- this will use fixed memory allocation functions, but allows us to avoid the FT_Init_FreeType and FT_Done_FreeType global mess.
new_face :: MonadIO m => Library -> FilePath -> Int -> m Face
new_face library path (fromIntegral -> face_index) = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error { FT_New_Face($library:library,$str:path,$(FT_Long face_index),$(FT_Face * p))}|] >>= ok
    peek p >>= foreignFace

-- | Add a reference to a face
-- 
-- For the most part this should already be done for you through the API provided in Haskell,
-- but you may need this if you transfer the face to another library.
reference_face :: MonadIO m => Face -> m ()
reference_face face = liftIO $ [C.exp|FT_Error { FT_Reference_Face($fptr-ptr:(FT_Face face))}|] >>= ok

-- | Remove a reference to a face
--
-- For the most part this should already be done for you through the API provided in Haskell,
-- but you may need this if you claim ownership of a face from another library.
done_face :: MonadIO m => Face -> m ()
done_face face = liftIO $ [C.exp|FT_Error { FT_Done_Face($fptr-ptr:(FT_Face face))}|] >>= ok
