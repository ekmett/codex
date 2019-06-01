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
module Data.Text.Bidirectional
( Bidi(..)
, open
, openSized
, countParagraphs
, countRuns
, getBaseDirection
, getDirection
, getLevelAt
, getLevels
, getLogicalIndex
, getLogicalMap
, getLogicalRun
, getParaLevel
, getParagraph
, getParagraphByIndex
, getProcessedLength
, getReorderingMode
, getReorderingOptions
, getResultLength
, getText
, getVisualRun
, getVisualIndex
, invertMap
, isInverse
, isOrderParagraphsLTR
, orderParagraphsLTR
, setContext
, setInverse
, setLine
, setPara
, setReorderingMode
, setReorderingOptions

--
, Level
  ( Level
  , DEFAULT_LTR
  , DEFAULT_RTL
  , MAX_EXPLICIT_LEVEL
  , LEVEL_OVERRIDE
  )
, isRTL, isLTR

, Direction(..)
, ReorderingMode(..)

, ReorderingOption
  ( ReorderingOption
  , OPTION_DEFAULT
  , OPTION_INSERT_MARKS
  , OPTION_REMOVE_CONTROLS
  , OPTION_STREAMING
  )

, UBiDi

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

#ifndef HLINT
pattern DEFAULT_LTR = Level (#const UBIDI_DEFAULT_LTR)
pattern DEFAULT_RTL = Level (#const UBIDI_DEFAULT_RTL)
pattern LEVEL_OVERRIDE = Level (#const UBIDI_LEVEL_OVERRIDE)
pattern MAX_EXPLICIT_LEVEL = Level (#const UBIDI_MAX_EXPLICIT_LEVEL)

-- | Special value which can be returned by the mapping functions when a logical index has no corresponding visual index or vice-versa.
-- Returned by 'getVisualIndex', 'getVisualMap', 'getLogicalIndex', 'getLogicalMap'
pattern MAP_NOWHERE = (#const UBIDI_MAP_NOWHERE) :: Int

pattern KEEP_BASE_COMBINING = (#const UBIDI_KEEP_BASE_COMBINING) :: Int
pattern DO_MIRRORING = (#const UBIDI_DO_MIRRORING) :: Int
pattern INSERT_LRM_FOR_NUMERIC = (#const UBIDI_INSERT_LRM_FOR_NUMERIC) :: Int
pattern REMOVE_BIDI_CONTROLS  = (#const UBIDI_REMOVE_BIDI_CONTROLS ) :: Int
pattern OUTPUT_REVERSE = (#const UBIDI_OUTPUT_REVERSE) :: Int
pattern BIDI_CLASS_DEFAULT = (#const U_BIDI_CLASS_DEFAULT) :: Int
#endif

newtype UErrorCode = UErrorCode Int32
  deriving (Eq,Ord,Show,Num,Enum,Real,Integral,Storable)

newtype ReorderingOption = ReorderingOption Int32
  deriving (Eq,Ord,Show,Bits)

pattern OPTION_DEFAULT = ReorderingOption (#const UBIDI_OPTION_DEFAULT)
pattern OPTION_INSERT_MARKS = ReorderingOption (#const UBIDI_OPTION_INSERT_MARKS)
pattern OPTION_REMOVE_CONTROLS = ReorderingOption (#const UBIDI_OPTION_REMOVE_CONTROLS)
pattern OPTION_STREAMING = ReorderingOption (#const UBIDI_OPTION_STREAMING)

instance Default ReorderingOption where
  def = OPTION_DEFAULT

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
        , (C.TypeName "UErrorCode", [t|UErrorCode|])
        , (C.TypeName "UChar", [t|Word16|])
        , (C.TypeName "UBool", [t|Int8|])
        ]
      , C.ctxAntiQuoters = Map.fromList
        [ ("bidi", anti (C.Ptr [] $ C.TypeSpecifier mempty $ C.TypeName "UBiDi") [t|Ptr UBiDi|] [|withBidi|])
        ]
      }

C.include "unicode/utypes.h"
C.include "unicode/uchar.h"
C.include "unicode/localpointer.h"
C.include "unicode/ubidi.h"

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
getText bidi = unsafeIOToPrim $
  withBidi bidi $ \p -> do
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

setReorderingOptions :: PrimMonad m => Bidi (PrimState m) -> ReorderingOption -> m ()
setReorderingOptions bidi options = unsafeIOToPrim [C.block|void { ubidi_setReorderingOptions($bidi:bidi,$(UBiDiReorderingOption options)); }|]

getReorderingOptions :: PrimMonad m => Bidi (PrimState m) -> m ReorderingOption
getReorderingOptions bidi = unsafeIOToPrim $ [C.exp|UBiDiReorderingOption { ubidi_getReorderingOptions($bidi:bidi) }|]

setContext :: PrimMonad m => Bidi (PrimState m) -> Text -> Text -> m ()
setContext bidi prologue_text epilogue_text = unsafeIOToPrim $ do
  useAsPtr prologue_text $ \prologue (fromIntegral -> prologue_len) ->
    useAsPtr epilogue_text $ \epilogue (fromIntegral -> epilogue_len) ->
      [C.block|UErrorCode {
        UErrorCode error_code;
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
      UErrorCode error_code;
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
    UErrorCode error_code;
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
      alloca $ \pErrorCode -> do
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
          UErrorCode error_code;
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
  alloca $ \pErrorCode ->
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

-- fun with name mangling
copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Ptr a -> Int -> m ()
copyPtrToMutablePrimArray (MutablePrimArray mba) (I## ofs) (Ptr addr) (I## n) =
  primitive_ $ \s -> case sizeOf## @a undefined of
    sz -> copyAddrToByteArray## addr mba (ofs *## sz) (n *## sz) s

invertMap :: PrimArray Int32 -> PrimArray Int32
invertMap pa = unsafePerformIO $ do -- use a full heavy weight dup check as this can be slow for large maps
  let !n = sizeofPrimArray pa
  let !m = fromIntegral (foldlPrimArray' max (-1) pa + 1)
  allocaArray (n+m) $ \srcMap -> do
    let dstMap = advancePtr srcMap n
    copyPrimArrayToPtr srcMap pa 0 n
    let len = fromIntegral n
    [C.block|void { ubidi_invertMap($(int32_t * srcMap),$(int32_t * dstMap),$(int32_t len)); }|]
    dst <- newPrimArray m
    copyPtrToMutablePrimArray dst 0 dstMap m
    unsafeFreezePrimArray dst

getVisualIndex :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m Int32
getVisualIndex bidi logicalIndex = unsafeIOToPrim $
  alloca $ \pErrorCode ->
    [C.exp|int32_t { 
      ubidi_getVisualIndex($bidi:bidi,$(int32_t logicalIndex),$(UErrorCode * pErrorCode))
    }|] <* (peek pErrorCode >>= ok)
  
getLogicalIndex :: PrimMonad m => Bidi (PrimState m) -> Int32 -> m Int32
getLogicalIndex bidi visualIndex = unsafeIOToPrim $
  alloca $ \pErrorCode ->
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
        UErrorCode error_code;
        ubidi_getLogicalMap($bidi:bidi,$(int32_t * indexMap),&error_code);
        return error_code;
      }|] >>= ok
      mpa <- newPrimArray len
      copyPtrToMutablePrimArray mpa 0 indexMap len
      unsafeFreezePrimArray mpa

getResultLength :: PrimMonad m => Bidi (PrimState m) -> m Int32
getResultLength bidi = unsafeIOToPrim [C.exp|int32_t { ubidi_getProcessedLength($bidi:bidi) }|]

getProcessedLength :: PrimMonad m => Bidi (PrimState m) -> m Int32
getProcessedLength bidi = unsafeIOToPrim [C.exp|int32_t { ubidi_getProcessedLength($bidi:bidi) }|]

getLevels :: PrimMonad m => Bidi (PrimState m) -> m (PrimArray Level)
getLevels bidi = stToPrim $ do
  len <- fromIntegral <$> getProcessedLength bidi
  unsafeIOToPrim $
    alloca $ \pErrorCode -> do
       levels <- [C.exp|const UBiDiLevel * { ubidi_getLevels($bidi:bidi, $(UErrorCode * pErrorCode)) }|]
       peek pErrorCode >>= ok
       mpa <- newPrimArray len
       copyPtrToMutablePrimArray mpa 0 levels len
       unsafeFreezePrimArray mpa
