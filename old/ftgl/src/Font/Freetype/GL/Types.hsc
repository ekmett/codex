{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Font.Freetype.GL.Types
  ( withSelf
  , TextureAtlas(..)
  , Box(..)
  , validBox
  , ftglCtx
  ) where

import Data.Coerce
import Data.Data (Data)
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

#include "vec234.h"

newtype TextureAtlas = TextureAtlas { getTextureAtlas :: ForeignPtr TextureAtlas }
  deriving (Eq,Ord,Show,Data)

data Box = Box { boxX, boxY, boxW, boxH :: {-# unpack #-} !Int }
  deriving (Eq,Ord,Show,Read,Data,Generic)

validBox :: Box -> Bool
validBox (Box x y w h) = x >= 0 && y >= 0 && w >= 0 && h >= 0

instance Storable Box where
  sizeOf _ = #size ivec4
  alignment _ = #alignment ivec4
  peek p = (\(x :: CInt) (y :: CInt) (w :: CInt) (h :: CInt) -> Box (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
    <$> (#peek ivec4, x_) p
    <*> (#peek ivec4, y_) p
    <*> (#peek ivec4, width) p
    <*> (#peek ivec4, height) p
  poke p (Box (fromIntegral -> (x :: CInt)) (fromIntegral -> (y :: CInt)) (fromIntegral -> (w :: CInt)) (fromIntegral -> (h :: CInt))) = do
    (#poke ivec4, x_) p x
    (#poke ivec4, y_) p y
    (#poke ivec4, width) p w
    (#poke ivec4, height) p h

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ C.unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

anti :: C.Type C.CIdentifier -> TH.TypeQ -> TH.ExpQ -> C.SomeAntiQuoter
anti cTy hsTyQ with = C.SomeAntiQuoter C.AntiQuoter
  { C.aqParser = do
    hId <- C.parseIdentifier
    let cId = C.mangleHaskellIdentifier hId
    return (cId, cTy, hId)
  , C.aqMarshaller = \_purity _cTypes _cTy cId -> do
    hsTy <- [t| Ptr $hsTyQ |]
    hsExp <- getHsVariable "fontConfigCtx" cId
    hsExp' <- [| $with (coerce $(pure hsExp)) |]
    return (hsTy, hsExp')
  }

ftglCtx :: C.Context
ftglCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "texture_atlas_t", [t| TextureAtlas |])
    , (C.TypeName "ivec4", [t| Box |])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr",        anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char (Just C.Unsigned)))) [t| CUChar |] [| withCUString |])
    , ("str",         anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char Nothing))) [t| CChar |] [| withCString |])
    , ("atlas",       anti (ptr (C.TypeName "texture_atlas_t")) [t| TextureAtlas |] [| withSelf |])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
