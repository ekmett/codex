{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
module Graphics.Harfbuzz.Private
( getHsVariable
, hs_free_stable_ptr
, withSelf, withPtr
, cbool, boolc, w2c, c2w
, newByteStringCStringLen
) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as Strict
import Data.Coerce
import Data.Word
import Data.Function ((&))
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.Haskell.TH as TH

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = TH.lookupValueName (C.unHaskellIdentifier s) >>= \ case
  Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++ ", because it's not in scope. (" ++ err ++ ")"
  Just hsName -> TH.varE hsName

foreign import ccall "&" hs_free_stable_ptr       :: FinalizerPtr ()

withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

withPtr :: Coercible a (Ptr a) => a -> (Ptr a -> IO r) -> IO r
withPtr = (&) . coerce

cbool :: CInt -> Bool
cbool = toEnum . fromIntegral

boolc :: Bool -> CInt
boolc = fromIntegral . fromEnum

w2c :: Word32 -> Char
w2c = toEnum . fromIntegral

c2w :: Char -> Word32
c2w = fromIntegral . fromEnum

-- | Copies 'ByteString' to newly allocated 'CString'. The result must be
-- | explicitly freed using 'free' or 'finalizerFree'.
newByteStringCStringLen :: Strict.ByteString -> IO CStringLen
newByteStringCStringLen (Strict.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    Strict.memcpy buf (p `plusPtr` o) l
    pokeByteOff buf l (0::Word8)
    return (castPtr buf, l)

