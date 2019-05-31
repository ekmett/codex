{-# language GeneralizedNewtypeDeriving #-}
{-# language ForeignFunctionInterface #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
{-# language CPP #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- FFI to the fontconfig library
--
-- As an internal module, I don't consider this module as supported by the PVP. Be careful.
module Graphics.Fontconfig.Internal
( Config(..)
, ObjectSet(..)
, Pattern(..)
, FontSet(..)
, SetName
  ( SetName
  , SetSystem
  , SetApplication
  )
, Stat(..), statCreate
, Cache(..)
, Range(..)
, CharSet(..)
, LangSet(..)
, StrSet(..)
, Matrix(..)
, Value

-- * Results
, StrList
, ValueBinding
  ( ValueBinding
  , ValueBindingWeak
  , ValueBindingStrong
  , ValueBindingSame
  )
, FcBool
  ( FcBool
  , FcFalse
  , FcTrue
  , FcDontCare
  )
, MatchKind
  ( MatchKind
  , MatchPattern
  , MatchFont
  , MatchScan
  )
, Spacing
  ( Spacing
  , MONO
  , DUAL
  , PROPORTIONAL
  , CHARCELL
  )
, LangResult
  ( LangResult
  , LangEqual
  , LangDifferentCountry
  , LangDifferentTerritory
  , LangDifferentLang
  )
, Type
  ( Type
  , TypeVoid
  , TypeInteger
  , TypeDouble
  , TypeString
  , TypeBool
  , TypeMatrix
  , TypeCharSet
  , TypeLangSet
  , TypeRange
  )
, Result(..)
, CResult
, getResult

, AllocationFailed(..)

-- * inline-c
, fontconfigCtx

-- * FFI
, foreignCache
, foreignCharSet
, foreignConfig
, foreignFontSet
, foreignLangSet
, foreignObjectSet
, foreignPattern
, foreignRange
, foreignStrSet

, _FcCharSetDestroy
, _FcConfigDestroy
, _FcDirCacheUnload
, _FcFontSetDestroy
, _FcLangSetDestroy
, _FcObjectSetDestroy
, _FcPatternDestroy
, _FcRangeDestroy
, _FcStrSetDestroy
, _FcValueDestroy
) where

import Control.Monad
import Data.Const.Unsafe
import Data.Data (Data)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Fontconfig.Private

newtype Config = Config { getConfig :: Maybe (ForeignPtr Config) } deriving (Eq, Ord, Show, Data)
newtype ObjectSet = ObjectSet { getObjectSet :: ForeignPtr ObjectSet } deriving (Eq, Ord, Show, Data)
newtype Pattern = Pattern { getPattern :: ForeignPtr Pattern } deriving (Eq, Ord, Show, Data)
newtype FontSet = FontSet { getFontSet :: ForeignPtr FontSet } deriving (Eq, Ord, Show, Data)
newtype Stat = Stat { getStat :: ForeignPtr Stat } deriving (Eq, Ord, Show, Data)
newtype Cache = Cache { getCache :: ForeignPtr Cache } deriving (Eq, Ord, Show, Data)
newtype Range = Range { getRange :: ForeignPtr Range } deriving (Eq, Ord, Show, Data)
newtype CharSet = CharSet { getCharSet :: ForeignPtr CharSet } deriving (Eq, Ord, Show, Data)
newtype LangSet = LangSet { getLangSet :: ForeignPtr LangSet } deriving (Eq, Ord, Show, Data)
newtype StrSet = StrSet { getStrSet :: ForeignPtr StrSet } deriving (Eq, Ord, Show, Data)

data Matrix = Matrix
  { matrix_xx
  , matrix_xy
  , matrix_yx
  , matrix_yy :: {-# unpack #-} !Double
  } deriving (Eq,Show,Data)

instance Storable Matrix where
  sizeOf _ = #size FcMatrix
  alignment _ = #alignment FcMatrix
  peek p = Matrix
    <$> (#peek FcMatrix, xx) p
    <*> (#peek FcMatrix, xy) p
    <*> (#peek FcMatrix, yx) p
    <*> (#peek FcMatrix, yy) p
  poke p Matrix{..} = do
    (#poke FcMatrix, xx) p matrix_xx
    (#poke FcMatrix, xy) p matrix_xy
    (#poke FcMatrix, yx) p matrix_yx
    (#poke FcMatrix, yy) p matrix_yy

#ifndef HLINT
newtype SetName = SetName CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern SetSystem = (#const FcSetSystem) :: SetName
pattern SetApplication = (#const FcSetSystem) :: SetName

newtype FcBool = FcBool CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern FcFalse = (#const FcFalse) :: FcBool
pattern FcTrue = (#const FcTrue) :: FcBool
pattern FcDontCare = (#const FcDontCare) :: FcBool

newtype MatchKind = MatchKind CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern MatchPattern = (#const FcMatchPattern) :: MatchKind
pattern MatchFont = (#const FcMatchFont) :: MatchKind
pattern MatchScan = (#const FcMatchScan) :: MatchKind

newtype LangResult = LangResult CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern LangEqual = (#const FcLangEqual) :: LangResult
pattern LangDifferentCountry = (#const FcLangDifferentCountry) :: LangResult
pattern LangDifferentTerritory = (#const FcLangDifferentTerritory) :: LangResult
pattern LangDifferentLang = (#const FcLangDifferentLang) :: LangResult

newtype ValueBinding = ValueBinding CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern ValueBindingWeak = (#const FcValueBindingWeak) :: ValueBinding
pattern ValueBindingStrong = (#const FcValueBindingStrong) :: ValueBinding
pattern ValueBindingSame = (#const FcValueBindingSame)  :: ValueBinding

newtype Spacing = Spacing CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern MONO = (#const FC_MONO) :: Spacing
pattern DUAL = (#const FC_DUAL) :: Spacing
pattern PROPORTIONAL = (#const FC_PROPORTIONAL) :: Spacing
pattern CHARCELL = (#const FC_CHARCELL) :: Spacing

newtype Type a = Type CInt deriving newtype (Eq,Ord,Show,Read,Enum,Num,Real,Integral,Storable)
pattern TypeVoid = (#const FcTypeVoid) :: Type ()
pattern TypeInteger = (#const FcTypeInteger) :: Type CInt
pattern TypeDouble = (#const FcTypeDouble) :: Type Double
pattern TypeString = (#const FcTypeString) :: Type (ConstPtr CUChar)
pattern TypeBool = (#const FcTypeBool) :: Type FcBool
pattern TypeMatrix = (#const FcTypeMatrix) :: Type (ConstPtr Matrix)
pattern TypeCharSet = (#const FcTypeCharSet) :: Type (ConstPtr CharSet)
pattern TypeLangSet = (#const FcTypeLangSet) :: Type (ConstPtr LangSet)
pattern TypeRange = (#const FcTypeRange) :: Type (ConstPtr Range)
-- pattern TypeFace provided by fontconfig-freetype
#endif

data StrList
data Value

-- newtype Value = Value { getValue :: ForeignPtr Value } deriving (Eq, Ord, Show, Data)

-- bootstrapping Storable Value
C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FcValue", [t| Value |])
    , (C.TypeName "FcMatrix", [t| Matrix |])
    , (C.TypeName "FcCharSet", [t| CharSet |])
    , (C.TypeName "FcLangSet", [t| LangSet |])
    , (C.TypeName "FcRange", [t| Range |])
    , (C.TypeName "FcChar8", [t| CUChar |])
    ]
  }

C.include "<fontconfig/fontconfig.h>"

#ifndef HLINT
#include <fontconfig/fontconfig.h>
#endif

instance Default Config where
  def = Config Nothing

statCreate :: IO Stat
statCreate = Stat <$> mallocForeignPtrBytes (#size struct stat)

-- * Results

data Result a
  = ResultMatch a
  | ResultNoMatch
  | ResultTypeMismatch
  | ResultNoId
  | ResultOutOfMemory
  deriving (Eq,Ord,Functor, Show,Read,Generic,Data)

instance Applicative Result where
  pure = ResultMatch
  (<*>) = ap

instance Monad Result where
  ResultMatch a >>= f = f a
  ResultNoMatch >>= _ = ResultNoMatch
  ResultTypeMismatch >>= _ = ResultTypeMismatch
  ResultNoId >>= _ = ResultNoId
  ResultOutOfMemory >>= _  = ResultOutOfMemory

type CResult = CInt

#ifndef HLINT
getResult :: Applicative f => CResult -> f r -> f (Result r)
getResult (#const FcResultMatch) m = ResultMatch <$> m
getResult (#const FcResultNoMatch) _ = pure ResultNoMatch
getResult (#const FcResultTypeMismatch) _ = pure ResultTypeMismatch
getResult (#const FcResultNoId) _ = pure ResultNoId
getResult (#const FcResultOutOfMemory) _ = pure ResultOutOfMemory
getResult _ _ = error "Font.Config.Internal.getResult: unknown result"
#endif

foreign import ccall "fontconfig/fontconfig.h &FcConfigDestroy" _FcConfigDestroy :: FinalizerPtr Config
foreign import ccall "fontconfig/fontconfig.h &FcObjectSetDestroy" _FcObjectSetDestroy:: FinalizerPtr ObjectSet
foreign import ccall "fontconfig/fontconfig.h &FcPatternDestroy" _FcPatternDestroy :: FinalizerPtr Pattern
foreign import ccall "fontconfig/fontconfig.h &FcFontSetDestroy" _FcFontSetDestroy :: FinalizerPtr FontSet
foreign import ccall "fontconfig/fontconfig.h &FcDirCacheUnload" _FcDirCacheUnload :: FinalizerPtr Cache
foreign import ccall "fontconfig/fontconfig.h &FcRangeDestroy" _FcRangeDestroy :: FinalizerPtr Range
foreign import ccall "fontconfig/fontconfig.h &FcCharSetDestroy" _FcCharSetDestroy :: FinalizerPtr CharSet
foreign import ccall "fontconfig/fontconfig.h &FcLangSetDestroy" _FcLangSetDestroy :: FinalizerPtr LangSet
foreign import ccall "fontconfig/fontconfig.h &FcStrSetDestroy" _FcStrSetDestroy :: FinalizerPtr StrSet
foreign import ccall "fontconfig/fontconfig.h &FcValueDestroy" _FcValueDestroy :: FinalizerPtr Value

-- * claim ownership of these objects by the GC

foreignCache :: Ptr Cache -> IO Cache
foreignCache = fmap Cache . newForeignPtr _FcDirCacheUnload

foreignCharSet :: Ptr CharSet -> IO CharSet
foreignCharSet = fmap CharSet . newForeignPtr _FcCharSetDestroy

foreignConfig :: Ptr Config -> IO Config
foreignConfig = fmap (Config . Just) . newForeignPtr _FcConfigDestroy

foreignFontSet :: Ptr FontSet -> IO FontSet
foreignFontSet = fmap FontSet . newForeignPtr _FcFontSetDestroy

foreignLangSet :: Ptr LangSet -> IO LangSet
foreignLangSet = fmap LangSet . newForeignPtr _FcLangSetDestroy

foreignObjectSet :: Ptr ObjectSet -> IO ObjectSet
foreignObjectSet = fmap ObjectSet . newForeignPtr _FcObjectSetDestroy

foreignPattern :: Ptr Pattern -> IO Pattern
foreignPattern = fmap Pattern . newForeignPtr _FcPatternDestroy

foreignRange :: Ptr Range -> IO Range
foreignRange = fmap Range . newForeignPtr _FcRangeDestroy

foreignStrSet :: Ptr StrSet -> IO StrSet
foreignStrSet = fmap StrSet . newForeignPtr _FcStrSetDestroy

-- * Inline C context

fontconfigCtx :: C.Context
fontconfigCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FcConfig", [t| Config |])
    , (C.TypeName "FcFontSet", [t| FontSet|])
    , (C.TypeName "FcObjectSet", [t| ObjectSet |])
    , (C.TypeName "FcPattern", [t| Pattern|])
    , (C.TypeName "FcCache", [t| Cache |])
    , (C.TypeName "FcBool", [t| FcBool |])
    , (C.TypeName "FcRange", [t| Range |])
    , (C.TypeName "FcChar8", [t| CUChar |])
    , (C.TypeName "FcChar16", [t| CUShort |])
    , (C.TypeName "FcChar32", [t| CUInt |])
    , (C.TypeName "FcCharSet", [t| CharSet |])
    , (C.TypeName "FcLangSet", [t| LangSet |])
    , (C.TypeName "FcStrSet", [t| StrSet |])
    , (C.TypeName "FcValue", [t| Value |])
    , (C.TypeName "FcStrList", [t| StrList |])
    , (C.Struct "stat", [t| Stat |])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr",        anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char (Just C.Unsigned)))) [t| Ptr CUChar |] [| withCUString |])
    , ("str",         anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char Nothing))) [t| Ptr CChar |] [| withCString |])
    , ("cache",       anti (ptr (C.TypeName "FcCache")) [t| Ptr Cache|] [| withForeignPtr |])
    , ("config",      anti (ptr (C.TypeName "FcConfig")) [t| Ptr Config |] [| withSelfMaybe |])
    , ("fontset",     anti (ptr (C.TypeName "FcFontSet")) [t| Ptr FontSet |] [| withForeignPtr |])
    , ("objectset",   anti (ptr (C.TypeName "FcObjectSet")) [t| Ptr ObjectSet |] [| withForeignPtr |])
    , ("charset",     anti (ptr (C.TypeName "FcCharSet")) [t| Ptr CharSet |] [| withForeignPtr |])
    , ("langset",     anti (ptr (C.TypeName "FcLangSet")) [t| Ptr LangSet |] [| withForeignPtr |])
    , ("strset",      anti (ptr (C.TypeName "FcStrSet")) [t| Ptr StrSet |] [| withForeignPtr |])
    , ("pattern",     anti (ptr (C.TypeName "FcPattern")) [t| Ptr Pattern |] [| withForeignPtr |])
    , ("matrix",      anti (ptr (C.TypeName "FcMatrix")) [t| Ptr Matrix |] [| with |])
    , ("range",       anti (ptr (C.TypeName "FcRange")) [t| Ptr Range |] [| withForeignPtr |])
    , ("stat",        anti (ptr (C.Struct "stat")) [t| Ptr Stat |] [| withForeignPtr |])
    , ("maybe-stat",  anti (ptr (C.Struct "stat")) [t| Ptr Stat |] [| maybeWith withForeignPtr |])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
