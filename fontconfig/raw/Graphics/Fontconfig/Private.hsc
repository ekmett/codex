{-# language LambdaCase #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveAnyClass #-}
{-# language ViewPatterns #-}
{-# options_haddock not-home #-}
module Graphics.Fontconfig.Private
( anti
, withSelfMaybe
, marshal
, unmarshal
, unmarshal'
, withCUString
, peekCUString
, check
, boolc, cbool
, AllocationFailed(..)
) where

import Control.Exception
import Control.Monad (unless)
import Data.Coerce
import Data.Data (Data)
import Data.Functor ((<&>))
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = TH.lookupValueName (C.unHaskellIdentifier s) >>= \ case
  Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++ ", because it's not in scope. (" ++ err ++ ")"
  Just hsName -> TH.varE hsName

anti :: C.Type C.CIdentifier -> TH.TypeQ -> TH.ExpQ -> C.SomeAntiQuoter
anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
  { C.aqParser = C.parseIdentifier <&> \hId -> (C.mangleHaskellIdentifier hId, cTy, hId)
  , C.aqMarshaller = \_ _ _ cId -> (,) <$> hsTyQ <*> [|$w (coerce $(getHsVariable "freeTypeCtx" cId))|]
  }

withSelfMaybe :: Coercible a (Maybe (ForeignPtr a)) => a -> (Ptr a -> IO r) -> IO r
withSelfMaybe a f = maybe (f nullPtr) (`withForeignPtr` f) (coerce a)

-- this allows for expansion or partial implementation of the list of alternatives
unmarshal :: forall a. (Enum a, Bounded a) => CInt -> Maybe a
unmarshal (fromIntegral -> m)
  | fromEnum (minBound :: a) <= m && m <= fromEnum (maxBound :: a) = Just (toEnum m)
  | otherwise = Nothing

unmarshal' :: Enum a => CInt -> a
unmarshal' = toEnum . fromIntegral

marshal :: Enum a => a -> CInt
marshal = fromIntegral . fromEnum

withCUString :: String -> (Ptr CUChar -> IO r) -> IO r
withCUString s f = withCString s (f . castPtr)

data AllocationFailed = AllocationFailed deriving (Show, Data, Exception)

check :: Bool -> IO ()
check b = unless b $ throwIO AllocationFailed

cbool :: CInt -> Bool
cbool = (0/=)

boolc :: Bool -> CInt
boolc = fromIntegral . fromEnum

peekCUString :: Ptr CUChar -> IO String
peekCUString = peekCString . castPtr

