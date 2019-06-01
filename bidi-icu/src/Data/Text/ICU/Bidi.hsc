{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}
{-# language BangPatterns #-}
{-# language MagicHash #-} -- has fun interactions with hsc2hs!
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}

module Data.Text.ICU.Bidi
( Bidi(..)
, pattern MAP_NOWHERE

, open
, openSized

, countParagraphs
, countRuns

, getCustomizedClass
, getLength
, getLevelAt
, getLevels
, getLogicalIndex
, getLogicalMap
, getLogicalRun
, getParaLevel
, getParagraph
, getParagraphByIndex
, getProcessedLength
, getResultLength
, getText
, getVisualIndex
, getVisualMap
, getVisualRun
, invertMap
, isInverse
, isOrderParagraphsLTR
, orderParagraphsLTR
, reorderLogical
, reorderVisual
, setContext
, setInverse
, setLine
, setPara
-- * Levels
, Level
  ( Level
  , DEFAULT_LTR
  , DEFAULT_RTL
  , MAX_EXPLICIT_LEVEL
  )
, isRTL, isLTR
, isOverride
, override
, pattern LEVEL_OVERRIDE
-- * Direction
, Direction(..)
, getBaseDirection
, getDirection
-- * Reordering
, ReorderingMode(..)
, getReorderingMode
, setReorderingMode
, ReorderingOption
  ( ReorderingOption
  , OPTION_DEFAULT
  , OPTION_INSERT_MARKS
  , OPTION_REMOVE_CONTROLS
  , OPTION_STREAMING
  )
, getReorderingOptions
, setReorderingOptions
-- * Character Direction Classes
, CharDirection
  ( CharDirection
  , LEFT_TO_RIGHT
  , RIGHT_TO_LEFT
  , EUROPEAN_NUMBER
  , EUROPEAN_NUMBER_SEPARATOR
  , EUROPEAN_NUMBER_TERMINATOR
  , ARABIC_NUMBER
  , COMMON_NUMBER_SEPARATOR
  , BLOCK_SEPARATOR
  , SEGMENT_SEPARATOR
  , WHITE_SPACE_NEUTRAL
  , OTHER_NEUTRAL
  , LEFT_TO_RIGHT_EMBEDDING
  , LEFT_TO_RIGHT_OVERRIDE
  , RIGHT_TO_LEFT_ARABIC
  , RIGHT_TO_LEFT_EMBEDDING
  , RIGHT_TO_LEFT_OVERRIDE
  , POP_DIRECTIONAL_FORMAT
  , DIR_NON_SPACING_MARK
  , BOUNDARY_NEUTRAL
  , FIRST_STRONG_ISOLATE
  , LEFT_TO_RIGHT_ISOLATE
  , RIGHT_TO_LEFT_ISOLATE
  , POP_DIRECTIONAL_ISOLATE
  , BIDI_CLASS_DEFAULT -- hack
  )

, ClassCallback
, mkClassCallback
, setClassCallback
, getClassCallback

-- * Writing
, WriteOptions
  ( WriteOptions
  , DO_MIRRORING
  , INSERT_LRM_FOR_NUMERIC
  , KEEP_BASE_COMBINING
  , REMOVE_BIDI_CONTROLS
  , OUTPUT_REVERSE
  )
, writeReordered
, writeReverse

-- * Internal
, UBiDi
, UErrorCode(..)
) where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Coerce
import Data.Data (Data)
import Data.Default
import Data.Functor ((<&>))
import Data.Int
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Text as Text
import Data.Text.Foreign as Text
import Data.Traversable (for)
import qualified Data.Vector.Primitive as Prim
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as Concurrent
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Arr (Ix)
import GHC.Generics (Generic)
import GHC.Prim
import GHC.Types
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- PrimArray utilities
--------------------------------------------------------------------------------

-- fun with name mangling
copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Ptr a -> Int -> m ()
copyPtrToMutablePrimArray (MutablePrimArray mba) (I## ofs) (Ptr addr) (I## n) =
  primitive_ $ \s -> case sizeOf## @a undefined of
    sz -> copyAddrToByteArray## addr mba (ofs *## sz) (n *## sz) s

withPrimArrayLen :: forall a r. Prim a => PrimArray a -> (Int -> Ptr a -> IO r) -> IO r
withPrimArrayLen pa k = allocaBytes (n * I## (sizeOf## @a undefined)) $ \p -> copyPrimArrayToPtr p pa 0 n *> k n p where
  n = sizeofPrimArray pa

peekPrimArray :: Prim a => Int -> Ptr a -> IO (PrimArray a)
peekPrimArray len ptr = do
  mpa <- newPrimArray len
  copyPtrToMutablePrimArray mpa 0 ptr len
  unsafeFreezePrimArray mpa

#ifndef HLINT
#include "unicode/utypes.h"
#include "unicode/uchar.h"
#include "unicode/localpointer.h"
#include "unicode/ubidi.h"
#endif

newtype Level = Level Word8
  deriving (Eq,Ord,Show,Storable,Prim)

isRTL :: Level -> Bool
isRTL = coerce (odd @Word8)

isLTR :: Level -> Bool
isLTR = coerce (even @Word8)

pattern LEVEL_OVERRIDE = (#const UBIDI_LEVEL_OVERRIDE) :: Word8

isOverride :: Level -> Bool
isOverride (Level l) = l .&. LEVEL_OVERRIDE /= 0

override :: Level -> Level
override (Level l) = Level (l .|. LEVEL_OVERRIDE)

#ifndef HLINT
pattern DEFAULT_LTR = Level (#const UBIDI_DEFAULT_LTR)
pattern DEFAULT_RTL = Level (#const UBIDI_DEFAULT_RTL)
pattern MAX_EXPLICIT_LEVEL = Level (#const UBIDI_MAX_EXPLICIT_LEVEL)
#endif

#ifndef HLINT
-- | Special value which can be returned by the mapping functions when a logical index has no corresponding visual index or vice-versa.
-- Returned by 'getVisualIndex', 'getVisualMap', 'getLogicalIndex', 'getLogicalMap'
pattern MAP_NOWHERE = (#const UBIDI_MAP_NOWHERE) :: Int
#endif

newtype WriteOptions = WriteOptions Int16
  deriving (Eq,Ord,Show,Storable,Prim,Bits)

#ifndef HLINT
pattern KEEP_BASE_COMBINING = WriteOptions (#const UBIDI_KEEP_BASE_COMBINING)
pattern DO_MIRRORING = WriteOptions (#const UBIDI_DO_MIRRORING)
pattern INSERT_LRM_FOR_NUMERIC = WriteOptions (#const UBIDI_INSERT_LRM_FOR_NUMERIC)
pattern REMOVE_BIDI_CONTROLS  = WriteOptions (#const UBIDI_REMOVE_BIDI_CONTROLS )
pattern OUTPUT_REVERSE = WriteOptions (#const UBIDI_OUTPUT_REVERSE)
#endif

instance Default WriteOptions where
  def = WriteOptions 0

newtype UErrorCode = UErrorCode Int32
  deriving (Eq,Ord,Show,Num,Enum,Real,Integral,Storable)

instance Default UErrorCode where
  def = UErrorCode 0

-- * Reordering Options

newtype ReorderingOption = ReorderingOption Int32
  deriving (Eq,Ord,Show,Bits)

#ifndef HLINT
pattern OPTION_DEFAULT = ReorderingOption (#const UBIDI_OPTION_DEFAULT)
pattern OPTION_INSERT_MARKS = ReorderingOption (#const UBIDI_OPTION_INSERT_MARKS)
pattern OPTION_REMOVE_CONTROLS = ReorderingOption (#const UBIDI_OPTION_REMOVE_CONTROLS)
pattern OPTION_STREAMING = ReorderingOption (#const UBIDI_OPTION_STREAMING)
#endif

instance Default ReorderingOption where
  def = OPTION_DEFAULT

-- * Character Directions

-- This is morally the same as text-icu's Direction type, but that one is missing a few definitions =(
-- See bos/text-icu#44

newtype CharDirection = CharDirection Int32 deriving
  (Eq,Ord,Show,Storable,Prim)

#ifndef HLINT
pattern LEFT_TO_RIGHT = CharDirection (#const U_LEFT_TO_RIGHT)
pattern RIGHT_TO_LEFT = CharDirection (#const U_RIGHT_TO_LEFT)
pattern EUROPEAN_NUMBER = CharDirection (#const U_EUROPEAN_NUMBER)
pattern EUROPEAN_NUMBER_SEPARATOR = CharDirection (#const U_EUROPEAN_NUMBER_SEPARATOR)
pattern EUROPEAN_NUMBER_TERMINATOR = CharDirection (#const U_EUROPEAN_NUMBER_TERMINATOR)
pattern ARABIC_NUMBER = CharDirection (#const U_ARABIC_NUMBER)
pattern COMMON_NUMBER_SEPARATOR = CharDirection (#const U_COMMON_NUMBER_SEPARATOR)
pattern BLOCK_SEPARATOR = CharDirection (#const U_BLOCK_SEPARATOR)
pattern SEGMENT_SEPARATOR = CharDirection (#const U_SEGMENT_SEPARATOR)
pattern WHITE_SPACE_NEUTRAL = CharDirection (#const U_WHITE_SPACE_NEUTRAL)
pattern OTHER_NEUTRAL = CharDirection (#const U_OTHER_NEUTRAL)
pattern LEFT_TO_RIGHT_EMBEDDING = CharDirection (#const U_LEFT_TO_RIGHT_EMBEDDING)
pattern LEFT_TO_RIGHT_OVERRIDE = CharDirection (#const U_LEFT_TO_RIGHT_OVERRIDE)
pattern RIGHT_TO_LEFT_ARABIC = CharDirection (#const U_RIGHT_TO_LEFT_ARABIC)
pattern RIGHT_TO_LEFT_EMBEDDING = CharDirection (#const U_RIGHT_TO_LEFT_EMBEDDING)
pattern RIGHT_TO_LEFT_OVERRIDE = CharDirection (#const U_RIGHT_TO_LEFT_OVERRIDE)
pattern POP_DIRECTIONAL_FORMAT = CharDirection (#const U_POP_DIRECTIONAL_FORMAT)
pattern DIR_NON_SPACING_MARK = CharDirection (#const U_DIR_NON_SPACING_MARK)
pattern BOUNDARY_NEUTRAL = CharDirection (#const U_BOUNDARY_NEUTRAL)
pattern FIRST_STRONG_ISOLATE = CharDirection (#const U_FIRST_STRONG_ISOLATE)
pattern LEFT_TO_RIGHT_ISOLATE = CharDirection (#const U_LEFT_TO_RIGHT_ISOLATE)
pattern RIGHT_TO_LEFT_ISOLATE = CharDirection (#const U_RIGHT_TO_LEFT_ISOLATE)
pattern POP_DIRECTIONAL_ISOLATE = CharDirection (#const U_POP_DIRECTIONAL_ISOLATE)
pattern BIDI_CLASS_DEFAULT = CharDirection (#const U_BIDI_CLASS_DEFAULT) -- a damn lie
#endif

type ClassCallback = Ptr () -> Int32 -> IO CharDirection

foreign import ccall "wrapper" mkClassCallback :: ClassCallback -> IO (FunPtr ClassCallback)

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
  , parentLink :: IORef (Maybe (Bidi s))  -- used to deal with ubidi_setLine shared content issues
  , getBidi :: ForeignPtr UBiDi
  }

withBidi :: Bidi s -> (Ptr UBiDi -> IO r) -> IO r
withBidi = withForeignPtr . getBidi

let
  anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
    { C.aqParser = C.parseIdentifier <&> \hId -> (C.mangleHaskellIdentifier hId, cTy, hId)
    , C.aqMarshaller = \_ _ _ cId -> (,) <$> hsTyQ <*> [|$w (coerce $(getHsVariable "bidirectionalCtx" cId))|]
    }
  getHsVariable err s = TH.lookupValueName (C.unHaskellIdentifier s) >>= \ case
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++ ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName
 in C.context $ C.baseCtx <> C.fptrCtx <> mempty
      { C.ctxTypesTable = Map.fromList
        [ (C.TypeName "UBiDi", [t|UBiDi|])
        , (C.TypeName "UBiDiDirection", [t|Int32|])
        , (C.TypeName "UBiDiLevel", [t|Level|])
        , (C.TypeName "UBiDiReorderingMode", [t|Int32|])
        , (C.TypeName "UBiDiReorderingOption", [t|ReorderingOption|])
        , (C.TypeName "UBiDiClassCallbackPtr", [t|FunPtr ClassCallback|])
        , (C.TypeName "UBool", [t|Int8|])
        , (C.TypeName "UChar", [t|Word16|])
        , (C.TypeName "UChar32", [t|Int32|])
        , (C.TypeName "UCharDirection", [t|CharDirection|])
        , (C.TypeName "UErrorCode", [t|UErrorCode|])
        , (C.TypeName "WriteOptions", [t|WriteOptions|])
        ]
      , C.ctxAntiQuoters = Map.fromList
        [ ("bidi", anti (C.Ptr [] $ C.TypeSpecifier mempty $ C.TypeName "UBiDi") [t|Ptr UBiDi|] [|withBidi|])
        ]
      }

C.include "HsFFI.h"
C.include "unicode/utypes.h"
C.include "unicode/uchar.h"
C.include "unicode/localpointer.h"
C.include "unicode/ubidi.h"

C.verbatim "typedef UBiDiClassCallback * UBiDiClassCallbackPtr;"
C.verbatim "typedef int16_t WriteOptions;"

instance Exception UErrorCode where
  displayException e = unsafeLocalState $ peekCString [C.pure|const char * { u_errorName($(UErrorCode e)) }|]

foreignBidi :: Ptr UBiDi -> IO (Bidi s)
foreignBidi self_ptr = do
  embeddings_ref <- newIORef nullPtr -- embeddingLevels
  parent_ref <- newIORef Nothing -- parentLink
  self_fptr <- Concurrent.newForeignPtr self_ptr $ do
    [C.block|void { ubidi_close($(UBiDi * self_ptr)); }|] -- delete self
    embeddings <- readIORef embeddings_ref -- clean up embeddings
    when (embeddings /= nullPtr) $ free embeddings
    -- garbage collecting the parent link will allow parent to now possibly be freed if it has no references
  pure $ Bidi embeddings_ref parent_ref self_fptr

bad :: UErrorCode -> Bool
bad e = [C.pure|int { U_FAILURE($(UErrorCode e)) }|] /= 0

ok :: UErrorCode -> IO ()
ok e = when (bad e) $ throwIO e

open :: PrimMonad m => m (Bidi (PrimState m))
open = unsafeIOToPrim $ [C.exp|UBiDi * { ubidi_open() }|] >>= foreignBidi

openSized :: PrimMonad m => Int32 -> Int32 -> m (Bidi (PrimState m))
openSized maxLength maxRunCount = unsafeIOToPrim $
  with def $ \pErrorCode -> do
    bidi <- [C.exp|UBiDi * { ubidi_openSized($(int32_t maxLength),$(int32_t maxRunCount),$(UErrorCode * pErrorCode)) }|]
    peek pErrorCode >>= ok
    foreignBidi bidi

getText :: PrimMonad m => Bidi (PrimState m) -> m Text
getText bidi = unsafeIOToPrim $
  withBidi bidi $ \p -> do
    cwstr <- [C.exp|const UChar * { ubidi_getText($(const UBiDi * p))}|]
    len <- [C.exp|int32_t { ubidi_getLength($(const UBiDi * p))}|]
    fromPtr cwstr (fromIntegral len)

getLength :: PrimMonad m => Bidi (PrimState m) -> m Int32
getLength bidi = unsafeIOToPrim [C.exp|int32_t { ubidi_getLength($bidi:bidi) }|]

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

setReorderingOptions :: PrimMonad m => Bidi (PrimState m) -> ReorderingOption -> m ()
setReorderingOptions bidi options = unsafeIOToPrim [C.block|void { ubidi_setReorderingOptions($bidi:bidi,$(UBiDiReorderingOption options)); }|]

getReorderingOptions :: PrimMonad m => Bidi (PrimState m) -> m ReorderingOption
getReorderingOptions bidi = unsafeIOToPrim $ [C.exp|UBiDiReorderingOption { ubidi_getReorderingOptions($bidi:bidi) }|]

setContext :: PrimMonad m => Bidi (PrimState m) -> Text -> Text -> m ()
setContext bidi prologue_text epilogue_text = unsafeIOToPrim $ do
  useAsPtr prologue_text $ \prologue (fromIntegral -> prologue_len) ->
    useAsPtr epilogue_text $ \epilogue (fromIntegral -> epilogue_len) ->
      [C.block|UErrorCode {
        UErrorCode error_code = 0;
        ubidi_setContext(
          $bidi:bidi,
          $(const UChar * prologue),
          $(int32_t prologue_len),
          $(const UChar * epilogue),
          $(int32_t epilogue_len),
          &error_code
        );
        return error_code;
      }|] >>= ok

setPara :: PrimMonad m => Bidi (PrimState m) -> Text -> Level -> Maybe (Prim.Vector Level) -> m ()
setPara bidi text paraLevel els = unsafeIOToPrim $
  useAsPtr text $ \t i16@(fromIntegral -> len) -> do
    (fromMaybe nullPtr -> u) <- for els $ \(Prim.Vector vofs vlen (ByteArray vba)) -> do
      u <- if vlen < len
           then callocBytes (fromIntegral i16)
           else mallocBytes (fromIntegral i16)
      u <$ copyPrimArrayToPtr u (PrimArray vba) vofs vlen -- missing from Data.Vector
    let n = fromIntegral i16
    [C.block|UErrorCode {
      UErrorCode error_code =0;
      ubidi_setPara(
        $bidi:bidi,
        $(const UChar * t),
        $(int32_t n),
        $(UBiDiLevel paraLevel),
        $(UBiDiLevel * u),
        &error_code
      );
      return error_code;
    }|] >>= ok
    v <- atomicModifyIORef (embeddingLevels bidi) $ \v -> (u, v)
    when (v /= nullPtr) $ free v

setLine :: PrimMonad m => Bidi (PrimState m) -> Int32 -> Int32 -> Bidi (PrimState m) -> m ()
setLine para start limit line = unsafeIOToPrim $ do
  [C.block|UErrorCode {
    UErrorCode error_code = 0;
    ubidi_setLine(
      $bidi:para,
      $(int32_t start),
      $(int32_t limit),
      $bidi:line,
      &error_code
    );
    return error_code;
  }|] >>= ok
  writeIORef (parentLink line) $ Just para -- prevents deallocation of the paragraph bidi before we at least repurpose the line

getDirection :: PrimMonad m => Bidi (PrimState m) -> m Direction
getDirection bidi = unsafeIOToPrim $ [C.exp|UBiDiDirection { ubidi_getDirection($bidi:bidi) }|] <&> toEnum . fromIntegral

getBaseDirection :: Text -> Direction
getBaseDirection text = unsafeLocalState $
  useAsPtr text $ \t (fromIntegral -> len) ->
    [C.exp|UBiDiDirection { ubidi_getBaseDirection($(const UChar * t),$(int32_t len)) }|] <&> toEnum . fromIntegral

getParaLevel :: PrimMonad m => Bidi (PrimState m) -> m Level
getParaLevel bidi = unsafeIOToPrim [C.exp|UBiDiLevel { ubidi_getParaLevel($bidi:bidi) }|]

countParagraphs :: PrimMonad m => Bidi (PrimState m) -> m Int32
countParagraphs bidi = unsafeIOToPrim [C.exp|int32_t { ubidi_countParagraphs($bidi:bidi) }|]

-- | Given a paragraph or line bidirectional object @bidi@, and a @charIndex@ into the text
-- in the range @0@ to @'getProcessedLength' bidi -1@, this will return
-- the index of the paragraph, the index of the first character in the text,
-- the index of the end of the paragraph, and the level of the paragraph.
--
-- If the paragraph index is known, it can be more efficient to use 'getParagraphByIndex'
getParagraph :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m (Int32, Int32, Int32, Level)
getParagraph bidi charIndex = unsafeIOToPrim $
  allocaArray 2 $ \pParaStart ->
    alloca $ \pParaLevel ->
      with def $ \pErrorCode -> do
        result <- [C.block|int32_t {
          int32_t * pPara = $(int32_t * pParaStart);
          return ubidi_getParagraph(
            $bidi:bidi,
            $(int32_t charIndex),
            pPara,
            pPara+1,
            $(UBiDiLevel * pParaLevel),
            $(UErrorCode * pErrorCode)
          );
        }|]
        peek pErrorCode >>= ok
        (,,,) result
          <$> peek pParaStart
          <*> peek (advancePtr pParaStart 1) -- pParaLimit
          <*> peek pParaLevel

getParagraphByIndex :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m (Int32, Int32, Level)
getParagraphByIndex bidi paragraphIndex = unsafeIOToPrim $
  allocaArray 2 $ \pParaStart ->
    alloca $ \pParaLevel -> do
        [C.block|UErrorCode {
          int32_t * pPara = $(int32_t * pParaStart);
          UErrorCode error_code = 0;
          ubidi_getParagraph(
            $bidi:bidi,
            $(int32_t paragraphIndex),
            pPara,
            pPara+1,
            $(UBiDiLevel * pParaLevel),
            &error_code
          );
          return error_code;
        }|] >>= ok
        (,,) <$> peek pParaStart
             <*> peek (advancePtr pParaStart 1) -- pParaLimit
             <*> peek pParaLevel


getLevelAt :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m Level
getLevelAt bidi charIndex = unsafeIOToPrim [C.exp|UBiDiLevel { ubidi_getLevelAt($bidi:bidi,$(int32_t charIndex)) }|]

getLogicalRun :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m (Int32, Level)
getLogicalRun bidi logicalPosition = unsafeIOToPrim $
  alloca $ \pLevel ->
    (,) <$> [C.block|int32_t {
              int32_t logicalLimit;
              ubidi_getLogicalRun(
                $bidi:bidi,
                $(int32_t logicalPosition),
                &logicalLimit,
                $(UBiDiLevel * pLevel)
              );
              return logicalLimit;
            }|]
        <*> peek pLevel

countRuns :: PrimMonad m => Bidi (PrimState m) -> m Int32
countRuns bidi = unsafeIOToPrim $
  with def $ \pErrorCode -> do
    [C.exp|int32_t {
      ubidi_countRuns($bidi:bidi, $(UErrorCode * pErrorCode))
    }|] <* (peek pErrorCode >>= ok)

-- | Get one run's logical start, length, and directionality which will be LTR or RTL.
--
-- 'countRuns' should be called before the runs are retrieved
getVisualRun :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m (Int32, Int32, Direction)
getVisualRun bidi runIndex = unsafeIOToPrim $
  allocaArray 2 $ \pLogicalStart -> do
    dir <- [C.block|UBiDiDirection {
      int32_t * pLogicalStart = $(int32_t * pLogicalStart);
      return ubidi_getVisualRun(
        $bidi:bidi,
        $(int32_t runIndex),
        pLogicalStart,
        pLogicalStart+1 /* pLength */
      );
    }|] <&> toEnum . fromIntegral
    logical_start <- peek pLogicalStart
    len <- peek (advancePtr pLogicalStart 1) -- pLength
    pure (logical_start, len, dir)

invertMap :: PrimArray Int32 -> PrimArray Int32
invertMap pa = unsafePerformIO $ do -- use a full heavy weight dup check as this can be slow for large maps
  let !n = sizeofPrimArray pa
  let !m = fromIntegral (foldlPrimArray' max (-1) pa + 1)
  allocaArray (n+m) $ \srcMap -> do
    copyPrimArrayToPtr srcMap pa 0 n
    let len = fromIntegral n
    [C.block|void {
      int32_t * srcMap = $(int32_t * srcMap);
      int32_t len = $(int32_t len);
      ubidi_invertMap(srcMap,srcMap+len,len);
    }|]
    peekPrimArray m (advancePtr srcMap n) -- dstMap

getVisualIndex :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m Int32
getVisualIndex bidi logicalIndex = unsafeIOToPrim $
  with def $ \pErrorCode ->
    [C.exp|int32_t {
      ubidi_getVisualIndex($bidi:bidi,$(int32_t logicalIndex),$(UErrorCode * pErrorCode))
    }|] <* (peek pErrorCode >>= ok)

getLogicalIndex :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m Int32
getLogicalIndex bidi visualIndex = unsafeIOToPrim $
  with def $ \pErrorCode ->
    [C.exp|int32_t {
      ubidi_getLogicalIndex($bidi:bidi,$(int32_t visualIndex),$(UErrorCode * pErrorCode))
    }|] <* (peek pErrorCode >>= ok)

getLogicalMap :: PrimMonad m => Bidi (PrimState m) -> m (PrimArray Int32)
getLogicalMap bidi = stToPrim $ do
  len <- fromIntegral <$> do
    opts <- getReorderingOptions bidi
    processed_len <- getProcessedLength bidi
    if opts .&. OPTION_INSERT_MARKS /= OPTION_DEFAULT
    then max processed_len <$> getResultLength bidi
    else pure processed_len
  unsafeIOToPrim $
    allocaArray len $ \ indexMap -> do
      [C.block|UErrorCode {
        UErrorCode error_code = 0;
        ubidi_getLogicalMap($bidi:bidi,$(int32_t * indexMap),&error_code);
        return error_code;
      }|] >>= ok
      peekPrimArray len indexMap

getVisualMap :: PrimMonad m => Bidi (PrimState m) -> m (PrimArray Int32)
getVisualMap bidi = stToPrim $ do
  len <- fromIntegral <$> do
    opts <- getReorderingOptions bidi
    result_len <- getResultLength bidi
    if opts .&. OPTION_INSERT_MARKS /= OPTION_DEFAULT
    then max result_len <$> getProcessedLength bidi
    else pure result_len
  unsafeIOToPrim $
    allocaArray len $ \ indexMap -> do
      [C.block|UErrorCode {
        UErrorCode error_code = 0;
        ubidi_getVisualMap($bidi:bidi,$(int32_t * indexMap),&error_code);
        return error_code;
      }|] >>= ok
      peekPrimArray len indexMap

getResultLength :: PrimMonad m => Bidi (PrimState m) -> m Int32
getResultLength bidi = unsafeIOToPrim [C.exp|int32_t { ubidi_getProcessedLength($bidi:bidi) }|]

getProcessedLength :: PrimMonad m => Bidi (PrimState m) -> m Int32
getProcessedLength bidi = unsafeIOToPrim [C.exp|int32_t { ubidi_getProcessedLength($bidi:bidi) }|]

getLevels :: PrimMonad m => Bidi (PrimState m) -> m (PrimArray Level)
getLevels bidi = stToPrim $ do
  len <- fromIntegral <$> getProcessedLength bidi
  unsafeIOToPrim $
    with def $ \pErrorCode -> do
       levels <- [C.exp|const UBiDiLevel * { ubidi_getLevels($bidi:bidi, $(UErrorCode * pErrorCode)) }|]
       peek pErrorCode >>= ok
       peekPrimArray len levels

reorderLogical :: PrimArray Level -> PrimArray Int32
reorderLogical pa = unsafePerformIO $
  withPrimArrayLen pa $ \n@(fromIntegral -> len) levels ->
    allocaArray n $ \indexMap ->
      [C.block|void {
        ubidi_reorderLogical($(const UBiDiLevel * levels),$(int32_t len),$(int32_t * indexMap));
      }|] *> peekPrimArray n indexMap

reorderVisual :: PrimArray Level -> PrimArray Int32
reorderVisual pa = unsafePerformIO $
  withPrimArrayLen pa $ \n@(fromIntegral -> len) levels ->
    allocaArray n $ \indexMap ->
      [C.block|void {
        ubidi_reorderVisual($(const UBiDiLevel * levels),$(int32_t len),$(int32_t * indexMap));
      }|] *> peekPrimArray n indexMap

getCustomizedClass :: PrimMonad m => Bidi (PrimState m) -> Char -> m CharDirection
getCustomizedClass bidi (fromIntegral . fromEnum -> c) = unsafeIOToPrim
  [C.exp|UCharDirection { ubidi_getCustomizedClass($bidi:bidi, $(UChar32 c)) }|]

setClassCallback :: PrimMonad m => Bidi (PrimState m) -> FunPtr ClassCallback -> Ptr () -> m (FunPtr ClassCallback, Ptr ())
setClassCallback bidi newFn newCtx = unsafeIOToPrim $
  alloca $ \oldFn ->
    alloca $ \oldCtx -> do
      [C.block|UErrorCode {
        UErrorCode error_code = 0;
        ubidi_setClassCallback(
          $bidi:bidi,
          $(UBiDiClassCallbackPtr newFn),
          $(const void * newCtx),
          $(UBiDiClassCallbackPtr * oldFn),
          $(const void ** oldCtx),
          &error_code
        );
        return error_code;
      }|] >>= ok
      (,) <$> peek oldFn <*> peek oldCtx

getClassCallback :: PrimMonad m => Bidi (PrimState m) -> m (FunPtr ClassCallback, Ptr ())
getClassCallback bidi = unsafeIOToPrim $
  alloca $ \fn->
    alloca $ \ctx ->
      (,) <$  [C.block|void { ubidi_getClassCallback($bidi:bidi,$(UBiDiClassCallbackPtr * fn),$(const void ** ctx)); }|]
          <*> peek fn
          <*> peek ctx

writeReordered :: PrimMonad m => Bidi (PrimState m) -> WriteOptions -> m Text
writeReordered bidi options = stToPrim $ do
  destSize@(fromIntegral -> len) <- if options .&. INSERT_LRM_FOR_NUMERIC /= def
         then do
            len <- getLength bidi
            runs <- countRuns bidi
            pure $ len + 2 * runs
         else getProcessedLength bidi
  unsafeIOToPrim $
    allocaArray len $ \ dest ->
      with def $ \pErrorCode -> do
        actual_len <- [C.exp|int32_t {
          ubidi_writeReordered($bidi:bidi,$(UChar * dest),$(int32_t destSize),$(WriteOptions options),$(UErrorCode * pErrorCode))
        }|]
        peek pErrorCode >>= ok
        fromPtr dest $ fromIntegral actual_len

writeReverse :: Text -> WriteOptions -> Text
writeReverse t options = unsafePerformIO $ do
  useAsPtr t $ \src i16@(fromIntegral -> n) ->
    allocaArray (fromIntegral i16) $ \ dest ->
      with def $ \pErrorCode -> do
        actual_len <- [C.block|int32_t {
          int32_t len = $(int32_t n);
          return ubidi_writeReverse($(const UChar * src),len,$(UChar * dest),len,$(WriteOptions options),$(UErrorCode * pErrorCode));
        }|]
        peek pErrorCode >>= ok
        fromPtr dest $ fromIntegral actual_len
