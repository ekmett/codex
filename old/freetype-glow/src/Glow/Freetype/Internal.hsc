module Glow.Freetype.Internal
  ( withSelf
  , TextureAtlas(..)
  ) where

import 

withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

newtype TextureAtlas = TextureAtlas { getTextureAtlas :: ForeignPtr TextureAtlas }
  deriving (Eq,Ord,Show,Data)
