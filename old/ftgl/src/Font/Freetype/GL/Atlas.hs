{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language QuasiQuotes #-}
module Font.Freetype.GL.Atlas
  ( Atlas(..)
  , newAtlas
  , setRegion
  , getRegion
  , clear
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Const.Unsafe
import Data.Functor
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Language.C.Inline as C

import Font.Freetype.GL.Types

C.context $ C.baseCtx <> ftglCtx
C.include "vec234.h"
C.include "texture-atlas.h"

newtype Atlas s = Atlas TextureAtlas

-- | @'newAtlas' width height depth@ creates a new, empty texture atlas with the given width, height and bit depth.
newAtlas :: PrimMonad m => Int -> Int -> Int -> m (Atlas (PrimState m))
newAtlas (fromIntegral -> w) (fromIntegral -> h) (fromIntegral -> d)
  = unsafeIOToPrim $ [C.exp|texture_atlas_t * { texture_atlas_new($(int w),$(int h),$(int d)) }|]  >>= fmap (Atlas . TextureAtlas) . newForeignPtr texture_atlas_delete

setRegion :: (PrimMonad m, APtr p) => Atlas (PrimState m) -> Box -> p C.CUChar -> Int -> m ()
setRegion (Atlas self) (Box (fromIntegral -> x) (fromIntegral -> y) (fromIntegral -> w) (fromIntegral -> h)) (unsafePtr -> d) (fromIntegral -> stride)
  = unsafeIOToPrim $ [C.block|void { texture_atlas_set_region($atlas:self,$(int x),$(int y),$(int w),$(int h),$(const unsigned char * d),$(int stride)); }|]

getRegion :: PrimMonad m => Atlas (PrimState m) -> Int -> Int -> m (Maybe Box)
getRegion (Atlas self) (fromIntegral -> w) (fromIntegral -> h)
  = unsafeIOToPrim $ alloca $ \is ->
      [C.block|void { *$(ivec4 * is) = texture_atlas_get_region($atlas:self,$(int w),$(int h)); }|] >> (peek is <&> \b -> Just b <* guard (validBox b))

-- | @'clear' atlas@ removes all allocated regions from the atlas.
clear :: PrimMonad m => Atlas (PrimState m) -> m ()
clear (Atlas self) = unsafeIOToPrim $ [C.block|void { texture_atlas_clear($atlas:self); }|]

foreign import ccall "texture-atlas.h &texture_atlas_delete" texture_atlas_delete :: FinalizerPtr TextureAtlas
