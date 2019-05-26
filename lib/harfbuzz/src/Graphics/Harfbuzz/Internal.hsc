{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
{-# language CPP #-}

-- | ffi to the harfbuzz library
--
-- As an internal module, I don't consider this module as supported by the PVP. Be careful.
module Graphics.Harfbuzz.Internal
  ( Blob(..)
  , MemoryMode
    ( MemoryMode
    , MEMORY_MODE_DUPLICATE
    , MEMORY_MODE_READONLY
    , MEMORY_MODE_WRITABLE
    , MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE
    )
  -- * internals
  , withSelf
  , cbool
  , newByteStringCStringLen
  , harfbuzzCtx
  ) where

import Data.ByteString as Strict
import Data.ByteString.Internal as Strict
import Foreign
import Data.Coerce
import Data.Data (Data)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Foreign.C
import Foreign.Marshal.Unsafe
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

#include <hb.h>

newtype Blob = Blob { getConfig :: ForeignPtr Blob } deriving (Eq, Ord, Show, Data)

newtype MemoryMode = MemoryMode CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

pattern MEMORY_MODE_DUPLICATE :: MemoryMode
pattern MEMORY_MODE_READONLY :: MemoryMode
pattern MEMORY_MODE_WRITABLE :: MemoryMode
pattern MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE :: MemoryMode

pattern MEMORY_MODE_DUPLICATE = #const HB_MEMORY_MODE_DUPLICATE
pattern MEMORY_MODE_READONLY = #const HB_MEMORY_MODE_READONLY
pattern MEMORY_MODE_WRITABLE = #const HB_MEMORY_MODE_WRITABLE
pattern MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE = #const HB_MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_blob_t", [t| Blob |])
    ]
  }

C.include "<hb.h>"

instance Default Blob where
  def = unsafeLocalState $ [C.exp|hb_blob_t * { hb_blob_get_empty() }|] >>= fmap Blob . newForeignPtr_ 
  {-# noinline def #-}

withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

cbool :: CInt -> Bool
cbool = toEnum . fromIntegral

-- | Copies 'ByteString' to newly allocated 'CString'. The result must be
-- | explicitly freed using 'free' or 'finalizerFree'.
newByteStringCStringLen :: Strict.ByteString -> IO CStringLen
newByteStringCStringLen (Strict.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    Strict.memcpy buf (p `plusPtr` o) l
    pokeByteOff buf l (0::Word8)
    return (castPtr buf, l)

-- * Inline C context

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ C.unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

anti :: C.Type C.CIdentifier -> TH.TypeQ -> TH.ExpQ -> C.SomeAntiQuoter
anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
  { C.aqParser = do
    hId <- C.parseIdentifier
    let cId = C.mangleHaskellIdentifier hId
    return (cId, cTy, hId)
  , C.aqMarshaller = \_purity _cTypes _cTy cId -> do
    hsTy <- [t| Ptr $hsTyQ |]
    hsExp <- getHsVariable "fontConfigCtx" cId
    hsExp' <- [| $w (coerce $(pure hsExp)) |]
    return (hsTy, hsExp')
  }

harfbuzzCtx :: C.Context
harfbuzzCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_blob_t", [t| Blob |])
    , (C.TypeName "hb_bool_t", [t| CInt |])
    , (C.TypeName "hb_memory_mode_t", [t| MemoryMode |])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr",        anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char (Just C.Unsigned)))) [t| CUChar |] [| withCUString |])
    , ("str",         anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char Nothing))) [t| CChar |] [| withCString |])
    , ("blob",        anti (ptr (C.TypeName "hb_blob_t")) [t| Blob |] [| withSelf |])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
