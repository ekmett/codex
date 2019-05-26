-- | Color32F
module UI.Text.Color
    ( Color(..), toC'vec4, withC'vec4
    ) where

import Bindings.FreetypeGL.Vec234 (C'vec4(..))
import Data.Default
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

data Color = Color {-# unpack #-} !Float {-# unpack #-} !Float {-# unpack #-} !Float {-# unpack #-} !Float
  deriving (Eq, Ord, Read, Show)

instance Default Color where
  def = Color 0 0 0 0

toC'vec4 :: Color -> C'vec4
toC'vec4 (Color r g b a) = C'vec4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

withC'vec4 :: Color -> (Ptr C'vec4 -> IO a) -> IO a
withC'vec4 rgba act = alloca $ \vec4 -> do
  poke vec4 (toC'vec4 rgba)
  act vec4
