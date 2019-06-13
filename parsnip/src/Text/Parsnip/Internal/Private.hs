{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language BlockArguments #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language UnliftedFFITypes #-}
-- | the proverbial junk drawer
module Text.Parsnip.Internal.Private
( io
, mutableByteArrayContents#
, pinnedByteArrayFromString0
, pinnedByteArrayFromStringN0
, c_memchr
, c_strlen
, c_strncmp
, pure_strlen
, cint
, csize
, mkBS
, ForeignString(..)
, packForeignString
, withForeignString
) where

import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as B
import Data.String
import Data.Word
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import GHC.ForeignPtr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import System.IO.Unsafe

io :: IO a -> State# s -> (# State# s, a #)
io = unsafeCoerce#

-- !(*@#U missing primitive
mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# arr = byteArrayContents# (unsafeCoerce# arr)

-- hacked to return in any region
pinnedByteArrayFromString0 :: String -> MutableByteArray RealWorld
pinnedByteArrayFromString0 xs = pinnedByteArrayFromStringN0 (length xs) xs

pinnedByteArrayFromStringN0 :: Int -> String -> MutableByteArray RealWorld
pinnedByteArrayFromStringN0 n ys = unsafeDupablePerformIO do
  marr <- newPinnedByteArray (n+1)
  let go !ix [] = if ix == n
        then writeByteArray marr ix (0 :: Word8)
        else fail "pinnedByteArrayFromStringN: list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeByteArray marr ix (B.c2w x)
          go (ix + 1) xs
        else fail "pinnedByteArrayFromStringN: list length greater than specified size"
  go 0 ys
  pure marr

-- | An _immutable_ foreign cstring. This is mostly useful for things like calling strstr through ffi
-- where the needle needs to be null terminated.
newtype ForeignString = ForeignString (ForeignPtr Word8)
  deriving (Eq,Ord)

instance Show ForeignString where
  showsPrec d (ForeignString fp) = showsPrec d $ unsafeDupablePerformIO $ withForeignPtr (castForeignPtr fp) peekCString

instance IsString ForeignString where
  fromString s = unsafeDupablePerformIO $ do
    cstr <- newCString s
    ForeignString <$> newForeignPtr_ (castPtr cstr)

packForeignString :: ByteString -> ForeignString
packForeignString (PS _fp (plusForeignPtr _fp -> fp) n) = unsafeDupablePerformIO do
  MutableByteArray mba <- newPinnedByteArray (n+1)
  let mpa = MutablePrimArray mba :: MutablePrimArray RealWorld Word8 -- See haskell/primitive#253
  withForeignPtr fp $ \p -> copyPtrToMutablePrimArray mpa 0 p n
  writePrimArray mpa 0 (0 :: Word8) -- null terminate
  -- PrimArray ba <- unsafeFreezePrimArray mpa
  pure $ ForeignString $ ForeignPtr (mutableByteArrayContents# mba) (PlainPtr mba)

withForeignString :: ForeignString -> (CString -> IO r) -> IO r
withForeignString (ForeignString fp) = withForeignPtr (castForeignPtr fp)

---------------------------------------------------------------------------------------
-- * C
---------------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memchr" c_memchr :: Addr# -> CInt -> CSize -> IO (Ptr ())
foreign import ccall unsafe "string.h strncmp" c_strncmp :: Addr# -> Addr# -> CSize -> IO CInt
foreign import ccall unsafe "string.h strlen" c_strlen :: Addr# -> IO CSize
foreign import ccall unsafe "string.h strlen" pure_strlen :: Addr# -> CSize

cint :: CInt -> Int#
cint (fromIntegral -> I# i) = i
{-# inline cint #-}

csize :: CSize -> Int#
csize (fromIntegral -> I# i) = i
{-# inline csize #-}

mkBS :: Addr# -> ForeignPtrContents -> Int# -> ByteString
mkBS b g l = PS (ForeignPtr b g) 0 (I# l)
{-# inline mkBS #-}

