{-# language RankNTypes #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}

module Foreign.Const.C.String 
  ( ConstCString
  , ConstCStringLen
  , ConstCWString
  , ConstCWStringLen
  , constCString
  , constCWString

  , ACString
  , ACWString

  , peekACAString
  , peekACAStringLen
  , peekACString
  , peekACStringLen
  , peekACWString
  , peekACWStringLen
  , withConstCAString
  , withConstCAStringLen
  , withConstCString
  , withConstCStringLen
  , withConstCWString
  , withConstCWStringLen
  ) where

--import Control.Arrow
import Data.Coerce
import Data.Type.Coercion
import Foreign.C.String
import Foreign.C.Types

import Data.Const.Unsafe

type family Unapply s :: * -> * where
  Unapply (p CChar) = p

type ConstCString = ConstPtr CChar
type ConstCStringLen = (ConstCString, Int)
type ConstCWString = ConstPtr CWchar
type ConstCWStringLen = (ConstCWString, Int)

type ACWString s = (s ~ Unapply s CWchar, APtr (Unapply s))
type ACString s = (s ~ Unapply s CChar, APtr (Unapply s))

constCString :: ACString s => s -> ConstCString
constCString = constant
{-# inline constCString #-}

constCWString :: ACWString s => s -> ConstCWString
constCWString = constant
{-# inline constCWString #-}

peekACAString :: forall s. ACString s => s -> IO String
peekACAString = gcoerceWith (unsafeCStringCoercion @s) $ coerce peekCAString
{-# inline peekACAString #-}

peekACAStringLen :: forall s. ACString s => (s,Int) -> IO String
peekACAStringLen = gcoerceWith (unsafeCStringCoercion @s) $ coerce peekCAStringLen
{-# inline peekACAStringLen #-}

peekACString :: forall s. ACString s => s -> IO String
peekACString = gcoerceWith (unsafeCStringCoercion @s) $ coerce peekCString
{-# inline peekACString #-}

peekACStringLen :: forall s. ACString s => (s,Int) -> IO String
peekACStringLen = gcoerceWith (unsafeCStringCoercion @s) $ coerce peekCStringLen
{-# inline peekACStringLen #-}

peekACWString :: forall s. ACWString s => s -> IO String
peekACWString = gcoerceWith (unsafeCWStringCoercion @s) $ coerce peekCWString
{-# inline peekACWString #-}

peekACWStringLen :: forall s. ACWString s => (s,Int) -> IO String
peekACWStringLen = gcoerceWith (unsafeCWStringCoercion @s) $ coerce peekCWStringLen
{-# inline peekACWStringLen #-}

unsafeCStringCoercion :: forall s. ACString s => Coercion CString s 
unsafeCStringCoercion = unsafePtrCoercion @(Unapply s) @CChar
{-# inline unsafeCStringCoercion #-}

unsafeCWStringCoercion :: forall s. ACWString s => Coercion CWString s 
unsafeCWStringCoercion = unsafePtrCoercion @(Unapply s) @CWchar
{-# inline unsafeCWStringCoercion #-}

withConstCAString :: forall a. String -> (ConstCString -> IO a) -> IO a
withConstCAString = coerce (withCAString @a)
{-# inline withConstCAString #-}

withConstCAStringLen :: forall a. String -> (ConstCStringLen -> IO a) -> IO a
withConstCAStringLen = coerce (withCAStringLen @a)
{-# inline withConstCAStringLen #-}

withConstCString :: forall a. String -> (ConstCString -> IO a) -> IO a
withConstCString = coerce (withCString @a)
{-# inline withConstCString #-}

withConstCStringLen :: forall a. String -> (ConstCStringLen -> IO a) -> IO a
withConstCStringLen = coerce (withCStringLen @a)
{-# inline withConstCStringLen #-}

withConstCWString :: forall a. String -> (ConstCWString -> IO a) -> IO a
withConstCWString = coerce (withCWString @a)
{-# inline withConstCWString #-}

withConstCWStringLen :: forall a. String -> (ConstCWStringLen -> IO a) -> IO a
withConstCWStringLen = coerce (withCWStringLen @a)
{-# inline withConstCWStringLen #-}
