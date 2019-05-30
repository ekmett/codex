{-# language RankNTypes #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}

module Data.Const.ByteString 
  ( packACString
  , packACStringLen
  , useAsConstCString
  , useAsConstCStringLen
  ) where

import Data.ByteString as Strict
import Data.Coerce
import Data.Type.Coercion

import Data.Const.Unsafe

useAsConstCString :: forall a. Strict.ByteString -> (ConstCString -> IO a) -> IO a
useAsConstCString = coerce (Strict.useAsCString @a)
{-# inline useAsConstCString #-}

useAsConstCStringLen:: forall a. Strict.ByteString -> (ConstCStringLen -> IO a) -> IO a
useAsConstCStringLen = coerce (Strict.useAsCStringLen @a)
{-# inline useAsConstCStringLen #-}

packACString :: forall s. ACString s => s -> IO ByteString
packACString = gcoerceWith (unsafeCStringCoercion @s) $ coerce $ Strict.packCString
{-# inline packACString #-}

packACStringLen :: forall s. ACString s => (s,Int) -> IO ByteString
packACStringLen = gcoerceWith (unsafeCStringCoercion @s) $ coerce $ Strict.packCStringLen
{-# inline packACStringLen #-}
