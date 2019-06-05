{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
module Graphics.Harfbuzz.Private
( getHsVariable
, hs_free_stable_ptr
, cbool, boolc, w2c, c2w
, newByteStringCStringLen
, unsafeStateVar
, peekVector
) where

import Control.Monad.ST.Unsafe
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as Strict
import Data.Primitive.StateVar
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.Haskell.TH as TH

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = TH.lookupValueName (C.unHaskellIdentifier s) >>= \ case
  Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++ ", because it's not in scope. (" ++ err ++ ")"
  Just hsName -> TH.varE hsName

foreign import ccall "&" hs_free_stable_ptr       :: FinalizerPtr ()

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

unsafeStateVar :: IO a -> (a -> IO ()) -> StateVar s a
unsafeStateVar g s = StateVar (unsafeIOToST g) (unsafeIOToST . s)

peekVector :: forall a. Storable a => Int -> Ptr a -> IO (Vector a)
peekVector n src = do
  let bytes = n * sizeOf @a undefined
  dest <- mallocPlainForeignPtrBytes bytes
  withForeignPtr dest (\ dp -> copyBytes dp src bytes)
  pure $ Vector.unsafeFromForeignPtr0 dest n
