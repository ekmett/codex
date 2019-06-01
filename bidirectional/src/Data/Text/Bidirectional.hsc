{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}
module Data.Text.Bidirectional
( Bidi(..)
, open
, openSized
, getText
, setInverse
, isInverse
, orderParagraphsLTR
, isOrderParagraphsLTR
, setReorderingMode
, getReorderingMode
--
, Direction(..)
, ReorderingMode(..)
, UBiDi

, pattern DEFAULT_LTR
, pattern DEFAULT_RTL
, pattern MAX_EXPLICIT_LEVEL
, pattern LEVEL_OVERRIDE
, pattern MAP_NOWHERE
, pattern KEEP_BASE_COMBINING
, pattern DO_MIRRORING
, pattern INSERT_LRM_FOR_NUMERIC
, pattern REMOVE_BIDI_CONTROLS 
, pattern OUTPUT_REVERSE
, pattern BIDI_CLASS_DEFAULT
) where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Data (Data)
import Data.Functor ((<&>))
import Data.Int
import qualified Data.Map as Map
import Data.Text as Text
import Data.Text.Foreign as Text
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Ptr
import Foreign.Storable
import GHC.Arr (Ix)
import GHC.Generics (Generic)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

#include "unicode/utypes.h"
#include "unicode/uchar.h"
#include "unicode/localpointer.h"
#include "unicode/ubidi.h"

pattern DEFAULT_LTR = (#const UBIDI_DEFAULT_LTR) :: Int
pattern DEFAULT_RTL = (#const UBIDI_DEFAULT_RTL) :: Int
pattern MAX_EXPLICIT_LEVEL = (#const UBIDI_MAX_EXPLICIT_LEVEL) :: Int
pattern LEVEL_OVERRIDE = (#const UBIDI_LEVEL_OVERRIDE) :: Int
pattern MAP_NOWHERE = (#const UBIDI_MAP_NOWHERE) :: Int
pattern KEEP_BASE_COMBINING = (#const UBIDI_KEEP_BASE_COMBINING) :: Int
pattern DO_MIRRORING = (#const UBIDI_DO_MIRRORING) :: Int
pattern INSERT_LRM_FOR_NUMERIC = (#const UBIDI_INSERT_LRM_FOR_NUMERIC) :: Int
pattern REMOVE_BIDI_CONTROLS  = (#const UBIDI_REMOVE_BIDI_CONTROLS ) :: Int
pattern OUTPUT_REVERSE = (#const UBIDI_OUTPUT_REVERSE) :: Int
pattern BIDI_CLASS_DEFAULT = (#const U_BIDI_CLASS_DEFAULT) :: Int

newtype UErrorCode = UErrorCode Int32
  deriving (Eq,Ord,Show,Num,Enum,Real,Integral,Storable)

ubool :: Int8 -> Bool
ubool = (0/=)

boolu :: Bool -> Int8
boolu = fromIntegral . fromEnum

data Direction
  = LTR
  | RTL
  | Mixed
  | Neutral
  deriving (Eq,Ord,Show,Ix,Enum,Bounded,Data,Generic)

data ReorderingMode
  = ReorderDefault
  | ReorderNumbersSpecial
  | ReorderGroupNumbersWithR
  | ReorderRunsOnly
  | ReorderInverseNumbersAsL
  | ReorderInverseLikeDirect
  | ReorderInverseForNumbersSpecial
  | ReorderCount 
  deriving (Eq,Ord,Show,Ix,Enum,Bounded,Data,Generic)

data UBiDi
newtype Bidi s = Bidi (ForeignPtr UBiDi)

C.context $ C.baseCtx <> C.fptrCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "UBiDi", [t|UBiDi|])
    , (C.TypeName "UBiDiReorderingMode", [t|CInt|])
    , (C.TypeName "int32_t", [t|Int32|])
    , (C.TypeName "UErrorCode", [t|UErrorCode|])
    , (C.TypeName "UChar", [t|Word16|])
    , (C.TypeName "UBool", [t|Int8|])
    ]
  }

C.include "unicode/utypes.h"
C.include "unicode/uchar.h"
C.include "unicode/localpointer.h"
C.include "unicode/ubidi.h"

foreign import ccall "ubidi.h &" ubidi_close :: FunPtr (Ptr UBiDi -> IO ())

instance Exception UErrorCode where
  displayException e = unsafeLocalState $ peekCString [C.pure|const char * { u_errorName($(UErrorCode e)) }|]

ok :: UErrorCode -> IO ()
ok e = do
  b <- [C.exp|int { U_FAILURE($(UErrorCode e)) }|]
  when (b /= 0) $ throw e

open :: PrimMonad m => m (Bidi (PrimState m))
open = unsafeIOToPrim $ [C.exp|UBiDi * { ubidi_open() }|] >>= fmap Bidi . newForeignPtr ubidi_close

openSized :: PrimMonad m => Int32 -> Int32 -> m (Bidi (PrimState m))
openSized maxLength maxRunCount = unsafeIOToPrim $
  alloca $ \pErrorCode -> do
    bidi <- [C.exp|UBiDi * { ubidi_openSized($(int32_t maxLength),$(int32_t maxRunCount),$(UErrorCode * pErrorCode)) }|]
    peek pErrorCode >>= ok
    Bidi <$> newForeignPtr ubidi_close bidi

getText :: PrimMonad m => Bidi (PrimState m) -> m Text
getText (Bidi fp) = unsafeIOToPrim $
  withForeignPtr fp $ \p -> do
    cwstr <- [C.exp|const UChar * { ubidi_getText($(const UBiDi * p))}|]
    len <- [C.exp|int32_t { ubidi_getLength($(const UBiDi * p))}|]
    fromPtr cwstr (fromIntegral len)

setInverse :: PrimMonad m => Bidi (PrimState m) -> Bool -> m ()
setInverse bidi (boolu -> b) = unsafeIOToPrim [C.block|void { ubidi_setInverse($fptr-ptr:(UBiDi * bidi),$(UBool b)); }|]

isInverse :: PrimMonad m => Bidi (PrimState m) -> m Bool
isInverse bidi = unsafeIOToPrim $ [C.exp|UBool { ubidi_isInverse($fptr-ptr:(UBiDi * bidi))}|] <&> ubool

orderParagraphsLTR :: PrimMonad m => Bidi (PrimState m) -> Bool -> m ()
orderParagraphsLTR bidi (boolu -> b) = unsafeIOToPrim [C.block|void { ubidi_orderParagraphsLTR($fptr-ptr:(UBiDi * bidi),$(UBool b)); }|]

isOrderParagraphsLTR :: PrimMonad m => Bidi (PrimState m) -> m Bool
isOrderParagraphsLTR bidi = unsafeIOToPrim $ [C.exp|UBool { ubidi_isOrderParagraphsLTR($fptr-ptr:(UBiDi * bidi)) }|] <&> ubool

setReorderingMode :: PrimMonad m => Bidi (PrimState m) -> ReorderingMode -> m ()
setReorderingMode bidi (fromIntegral . fromEnum -> mode) = unsafeIOToPrim [C.block|void { ubidi_setReorderingMode($fptr-ptr:(UBiDi * bidi),$(UBiDiReorderingMode mode)); }|]

getReorderingMode :: PrimMonad m => Bidi (PrimState m) -> m ReorderingMode
getReorderingMode bidi = unsafeIOToPrim $ [C.exp|UBiDiReorderingMode{ ubidi_getReorderingMode($fptr-ptr:(UBiDi * bidi))}|] <&> toEnum . fromIntegral

