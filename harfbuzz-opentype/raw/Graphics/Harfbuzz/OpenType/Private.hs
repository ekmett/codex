{-# language FlexibleContexts #-}
module Graphics.Harfbuzz.OpenType.Private
( pump
, cbool, boolc
, withSelf
) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Functor ((<&>))
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

pump :: PrimMonad m => CUInt -> (CUInt -> CUInt -> IO (CUInt,CUInt,[a])) -> m [a]
pump n f = unsafeIOToPrim $ do
  (tot,ret,cs) <- f 0 n
  if tot == ret then pure cs else f n (tot - n) <&> \(_,_,ds) -> cs ++ ds

cbool :: CInt -> Bool
cbool = toEnum . fromIntegral

boolc :: Bool -> CInt
boolc = fromIntegral . fromEnum

withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce
