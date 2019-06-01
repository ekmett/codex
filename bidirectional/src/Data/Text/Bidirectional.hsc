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
, setContext
, setPara
, setLine
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
import Data.Vector.Primitive as Prim
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
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

#include "unicode/utypes.h"
#include "unicode/uchar.h"
#include "unicode/localpointer.h"
#include "unicode/ubidi.h"


pattern MAP_NOWHERE = (#const UBIDI_MAP_NOWHERE) :: Int
pattern KEEP_BASE_COMBINING = (#const UBIDI_KEEP_BASE_COMBINING) :: Int
pattern DO_MIRRORING = (#const UBIDI_DO_MIRRORING) :: Int
pattern INSERT_LRM_FOR_NUMERIC = (#const UBIDI_INSERT_LRM_FOR_NUMERIC) :: Int
pattern REMOVE_BIDI_CONTROLS  = (#const UBIDI_REMOVE_BIDI_CONTROLS ) :: Int
pattern OUTPUT_REVERSE = (#const UBIDI_OUTPUT_REVERSE) :: Int
pattern BIDI_CLASS_DEFAULT = (#const U_BIDI_CLASS_DEFAULT) :: Int

newtype Level = Level Word8
  deriving (Eq,Ord,Show,Num,Enum,Real,Integral,Storable)

pattern DEFAULT_LTR = (#const UBIDI_DEFAULT_LTR) :: Level
pattern DEFAULT_RTL = (#const UBIDI_DEFAULT_RTL) :: Level
pattern LEVEL_OVERRIDE = (#const UBIDI_LEVEL_OVERRIDE) :: Level
pattern MAX_EXPLICIT_LEVEL = (#const UBIDI_MAX_EXPLICIT_LEVEL) -- not tied to level

isRTL :: Level -> Bool
isRTL = odd

isLTR :: Level -> Bool
isLTR = even

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

data Bidi s = Bidi
  { embeddingLevels :: IORef (Ptr Level)  -- used to deal with ubidi_setPara shared content issues
  , parentLink :: IORef (Bidi s)          -- used to deal with ubidi_setLine shared content issues
  , getBidi :: ForeignPtr UBiDi
  }

C.context $ C.baseCtx <> C.fptrCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "UBiDi", [t|UBiDi|])
    , (C.TypeName "UBiDiReorderingMode", [t|CInt|])
    , (C.TypeName "UBiDiLevel", [t|Level|])
    , (C.TypeName "int32_t", [t|Int32|])
    , (C.TypeName "UErrorCode", [t|UErrorCode|])
    , (C.TypeName "UChar", [t|Word16|])
    , (C.TypeName "UBool", [t|Int8|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("bidi", anti (C.Ptr [] $ C.TypeSpecifier mempty $ C.TypeName "UBiDi") [t|Ptr UBiDi|] [|withForeignPtr . getBidi|]
  } where
  anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
    { C.aqParser = C.parseIdentifier <&> \hId -> (C.mangleHaskellIdentifier hId, cTy, hId)
    , C.aqMarshaller = \_ _ _ cId -> (,) <$> hsTyQ <*> [|$w (coerce $(getHsVariable "bidirectionalCtx" cId))|]
    }
  getHsVariable err s = TH.lookupValueName (C.unHaskellIdentifier s) >>= \ case
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++ ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

C.include "unicode/utypes.h"
C.include "unicode/uchar.h"
C.include "unicode/localpointer.h"
C.include "unicode/ubidi.h"

foreign import ccall "ubidi.h &" ubidi_close :: FunPtr (Ptr UBiDi -> IO ())

instance Exception UErrorCode where
  displayException e = unsafeLocalState $ peekCString [C.pure|const char * { u_errorName($(UErrorCode e)) }|]

foreignBidi :: Ptr Bidi -> IO Bidi
foreignBidi ptr p = do
  r <- newIORef nullPtr
  p <- newIORef undefined
  result <- Bidi r p <$> Concurrent.newForeignPtr (do ubidi_close p; v <- readIORef r; when (v /= nullPtr) $ free v) p
  result <$ writeIORef p result

ok :: UErrorCode -> IO ()
ok e = do
  b <- [C.exp|int { U_FAILURE($(UErrorCode e)) }|]
  when (b /= 0) $ throw e

open :: PrimMonad m => m (Bidi (PrimState m))
open = unsafeIOToPrim $ [C.exp|UBiDi * { ubidi_open() }|] >>= foreignBidi

openSized :: PrimMonad m => Int32 -> Int32 -> m (Bidi (PrimState m))
openSized maxLength maxRunCount = unsafeIOToPrim $
  alloca $ \pErrorCode -> do
    bidi <- [C.exp|UBiDi * { ubidi_openSized($(int32_t maxLength),$(int32_t maxRunCount),$(UErrorCode * pErrorCode)) }|]
    peek pErrorCode >>= ok
    foreignBidi bidi

getText :: PrimMonad m => Bidi (PrimState m) -> m Text
getText (Bidi _ fp) = unsafeIOToPrim $
  withForeignPtr fp $ \p -> do
    cwstr <- [C.exp|const UChar * { ubidi_getText($(const UBiDi * p))}|]
    len <- [C.exp|int32_t { ubidi_getLength($(const UBiDi * p))}|]
    fromPtr cwstr (fromIntegral len)

setInverse :: PrimMonad m => Bidi (PrimState m) -> Bool -> m ()
setInverse bidi (boolu -> b) = unsafeIOToPrim [C.block|void { ubidi_setInverse($bidi:bidi,$(UBool b)); }|]

isInverse :: PrimMonad m => Bidi (PrimState m) -> m Bool
isInverse bidi = unsafeIOToPrim $ [C.exp|UBool { ubidi_isInverse($bidi:bidi) }|] <&> ubool

orderParagraphsLTR :: PrimMonad m => Bidi (PrimState m) -> Bool -> m ()
orderParagraphsLTR bidi (boolu -> b) = unsafeIOToPrim [C.block|void { ubidi_orderParagraphsLTR($bidi:bidi,$(UBool b)); }|]

isOrderParagraphsLTR :: PrimMonad m => Bidi (PrimState m) -> m Bool
isOrderParagraphsLTR bidi = unsafeIOToPrim $ [C.exp|UBool { ubidi_isOrderParagraphsLTR($bidi:bidi) }|] <&> ubool

setReorderingMode :: PrimMonad m => Bidi (PrimState m) -> ReorderingMode -> m ()
setReorderingMode bidi (fromIntegral . fromEnum -> mode) = unsafeIOToPrim [C.block|void { ubidi_setReorderingMode($bidi:bidi,$(UBiDiReorderingMode mode)); }|]

getReorderingMode :: PrimMonad m => Bidi (PrimState m) -> m ReorderingMode
getReorderingMode bidi = unsafeIOToPrim $ [C.exp|UBiDiReorderingMode{ ubidi_getReorderingMode($bidi:bidi)}|] <&> toEnum . fromIntegral

setReorderingOptions :: PrimMonad m => Bidi (PrimState m) -> Word32 -> m ()
setReorderingOptions bidi options = unsafeIOToPrim [C.block|void { ubidi_setReorderingOptions($bidi:bidi,$(uint32_t options)); }|]

getReorderingOptions :: PrimMonad m => Bidi (PrimState m) -> m Word32
getReorderingOptions bidi = unsafeIOToPrim $ [C.exp|UBiDiReorderingOptions{ ubidi_getReorderingOptions($bidi:bidi)}|]

setContext :: PrimMonad m => Bidi (PrimState m) -> Text -> Text -> m ()
setContext bidi prologue_text epilogue_text = unsafeIOToPrim $ do
  useAsPtr prologue_text $ \prologue (fromIntegral prologue_len) ->
    useAsPtr epilogue_text $ \epilogue (fromIntegral epilogue_len) ->
      [C.block|UErrorCode {
        UErrorCode error_code;
        ubidi_setContext(
          $bidi:bidi,
          $(const UChar * prologue),
          $(int32_t prologue_len),
          $(const UChar * epilogue),
          $(int32_t epilogue_len),
          &error_code;
        );
        return error_code;
      }|] >>= ok

setPara :: PrimMonad m => Bidi (PrimState m) -> Text -> Level -> Maybe (Prim.Vector Level) -> m ()
setPara bidi text paraLevel els = unsafeIOToPrim $
  useAsPtr text $ \t (fromIntegral len) -> do
    u <- help els
    [C.block|UErrorCode {
      UErrorCode error_code;
      ubidi_setPara(
        $bidi:bidi
        $(const UChar * t),
        $(int32_t len),
        $(UBiDiLevel paraLevel),
        NULL,
        &error_code
      );
      return error_code;
    }|] >>= ok
    v <- atomicModifyIORef (embeddingLevels bidi) $ \v -> (u, v)
    when (v /= nullPtr) $ free v
  where
    help = maybe (pure nullPtr) $ \(Prim.Vector ofs vlen (PrimArray -> pa)) -> do
      u <- if vlen < len then callocBytes n else mallocBytes n
      u <$ copyPrimArrayToPtr u pa vofs vlen

setLine :: PrimMonad m => Bidi (PrimState m) -> Int32 -> Int32 -> Bidi (PrimState m) -> m ()
setLine para start limit line = unsafeIOToPrim $
  [C.block|UErrorCode {
    UErrrCode error_code;
    ubidi_setLine(
      $bidi:para,
      $(int32_t start),
      $(int32_t limit),
      $bidi:line,
      &error_code
    );
    return error_code;
  }|] >>= ok
  writeIORef (parentLink line) para -- prevents deallocation of the paragraph bidi before we at least repurpose the line

-- getDirection :: PrimMonad m => Bidi (PrimState m) -> m Direction
-- getBaseDirection :: PrimMonad m => Text -> Direction
-- getParaLevel :: PrimMonad m => Bidi (PrimState m) -> m Level
-- countParagraphs :: PrimMonad m => Bidi -> Int32
-- getLevelAt :: Bidi -> Int32 -> m Level
-- getLogicalRun
-- getVisualRun
-- getVisualIndex
