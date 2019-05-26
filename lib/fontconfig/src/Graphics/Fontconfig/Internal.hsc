{-# language GeneralizedNewtypeDeriving #-}
{-# language ForeignFunctionInterface #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
{-# language CPP #-}

-- | ffi to the fontconfig library
--
-- As an internal module, I don't consider this module as supported by the PVP. Be careful.
module Graphics.Fontconfig.Internal
  ( Config(..)
  , ObjectSet(..)
  , Pattern(..)
  , FontSet(..)
  , SetName(..)
  , Stat(..), statCreate
  , Cache(..)
  , Range(..)
  , CharSet(..)
  , LangSet(..)
  , StrSet(..)
  , Face(..)
  , Matrix(..)
  , Value(..)
  -- * Results
  , StrList
  , FcBool(..)
  , MatchKind(..)
  , Spacing(..)
  , LangResult(..)
  , marshal, unmarshal, unmarshal'
  , Result(..), CResult, getResult
  , AllocationFailed(..)
  -- * inline-c
  , fontConfigCtx
  -- * utilities
  , withSelf, withMaybeSelf, withSelfMaybe
  , check, cbool, boolc, peekCUString
  ) where

import Control.Exception
import Control.Monad
import Data.Coerce
import Data.Const.Unsafe
import Data.Data (Data)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Arr (Ix)
import GHC.Generics (Generic)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
#if USE_FREETYPE
import Graphics.Freetype.Internal
#endif


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

#if !USE_FREETYPE
newtype Face = Face { getFace :: ForeignPtr Face } deriving (Eq,Ord,Show,Data)
#endif

newtype Matrix = Matrix { getMatrix:: ForeignPtr Matrix } deriving (Eq,Ord,Show,Data) -- TODO use a struct and store it

newtype SetName = SetName CInt deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Storable)

pattern SetSystem :: SetName
pattern SetApplication :: SetName

pattern SetSystem = #const FcSetSystem
pattern SetApplication = #const FcSetSystem

newtype FcBool = FcBool CInt deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Storable)

pattern FcFalse :: FcBool
pattern FcTrue :: FcBool
pattern FcDontCare :: FcBool

pattern FcFalse = #const FcFalse
pattern FcTrue = #const FcTrue
pattern FcDontCare = #const FcDontCare

data MatchKind = MatchPattern | MatchFont | MatchScan
  deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Storable)

pattern MatchPattern :: MatchKind
pattern MatchFont :: MatchKind
pattern MatchScan :: MatchKind

pattern MatchPattern = #const FcMatchPattern
pattern MatchFont = #const FcMatchFont 
pattern MatchScan = #const FcMatchScan 

newtype LangResult = LangResult CInt

pattern LangEqual :: LangResult
pattern LangDifferentCountry :: LangResult
pattern LangDifferentTerritory :: LangResult
pattern LangDifferentLang :: LangResult

pattern LangEqual = #const FcLangEqual
pattern LangDifferentCountry = #const FcLangDifferentCountry
pattern LangDifferentTerritory = #const FcLangDifferentTerritory
pattern LangDifferentLang = #const FcLangDifferentLang

newtype ValueBinding = ValueBinding CInt
  deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Storable)

pattern ValueBindingWeak :: ValueBinding
pattern ValueBindingStrong :: ValueBinding
pattern ValueBindingSame :: ValueBinding

pattern ValueBindingWeak = #const FcValueBindingWeak
pattern ValueBindingStrong = #const FcValueBindingStrong
pattern ValueBindingSame = #const FcValueBindingSame

newtype Spacing = Spacing CInt
  deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Storable)

pattern MONO :: Spacing
pattern DUAL :: Spacing
pattern PROPORTIONAL :: Spacing
pattern CHARCELL :: Spacing

pattern MONO = #const FC_MONO
pattern DUAL = #const FC_DUAL
pattern PROPORTIONAL = #const FC_PROPORTIONAL
pattern CHARCELL = #const FC_CHARCELL

data StrList

-- newtype Value = Value { getValue :: ForeignPtr Value } deriving (Eq, Ord, Show, Data)
data Value
  = ValueUnknown
  | ValueVoid    !(Ptr ())
  | ValueInteger !Int
  | ValueDouble  !Double
  | ValueString  !(ConstPtr CUChar)
  | ValueBool    !FcBool
  | ValueMatrix  !(ConstPtr Matrix)
  | ValueCharSet !(ConstPtr CharSet)
  | ValueFace    !(ConstPtr Face)
  | ValueLangSet !(ConstPtr LangSet)
  | ValueRange   !(ConstPtr Range)

-- bootstrapping Storable Value
C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FcValue", [t| Value |])
    , (C.TypeName "FcMatrix", [t| Matrix |])
    , (C.TypeName "FcCharSet", [t| CharSet |])
    , (C.TypeName "FT_Face", [t| Face |])
    , (C.TypeName "FcLangSet", [t| LangSet |])
    , (C.TypeName "FcRange", [t| Range |])
    , (C.TypeName "FcChar8", [t| CUChar |])
    ]
  }

C.include "<fontconfig/fontconfig.h>"
#if USE_FREETYPE
C.include "<fontconfig/fcfreetype.h>"
#endif

#ifndef HLINT
#include <fontconfig/fontconfig.h>
# if USE_FREETYPE
#  include <fontconfig/fcfreetype.h>
# endif
#endif

instance Storable Value where
  sizeOf _ = #size FcValue
  alignment _ = #alignment FcValue
  poke v = \case
    ValueUnknown -> [C.block|void { $(FcValue *v)->type = FcTypeUnknown; }|]
    ValueVoid f -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeVoid; v->u.f = $(void*f); }|]
    ValueInteger (fromIntegral -> i) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeInteger; v->u.i = $(int i); }|]
    ValueDouble (coerce -> d) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeDouble;  v->u.d = $(double d); }|]
    ValueString (unsafePtr -> s) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeString; v->u.s = $(const FcChar8 * s); }|]
    ValueBool (marshal -> b) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeBool;    v->u.b = $(int b); }|]
    ValueMatrix (unsafePtr -> m) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeMatrix; v->u.m = $(const FcMatrix * m); }|]
    ValueCharSet (unsafePtr -> c) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeCharSet; v->u.c = $(const FcCharSet * c); }|]
    ValueFTFace (unsafePtr -> f) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeVoid; v->u.f = (void*)($(const FT_Face *f));}|]
    ValueLangSet (unsafePtr -> l) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeLangSet; v->u.l = $(const FcLangSet * l); }|]
    ValueRange (unsafePtr -> r) -> [C.block|void { FcValue*v = $(FcValue*v); v->type = FcTypeRange; v->u.r = $(const FcRange * r); }|]
  peek v = [C.exp|int { $(FcValue*v)->type } |] >>= \case
    (#const FcTypeVoid) -> ValueVoid <$> [C.exp|void* { $(FcValue*v)->u.f }|]
    (#const FcTypeInteger) -> ValueInteger . fromIntegral <$> [C.exp|int { $(FcValue*v)->u.i }|]
    (#const FcTypeDouble) -> ValueDouble . coerce <$> [C.exp|double { $(FcValue*v)->u.d }|]
    (#const FcTypeString) -> ValueString . ConstPtr <$> [C.exp|const FcChar8 * { $(FcValue*v)->u.s }|]
    (#const FcTypeBool) -> ValueBool . unmarshal' <$> [C.exp|int { $(FcValue*v)->u.b }|]
    (#const FcTypeMatrix) -> ValueMatrix . ConstPtr  <$> [C.exp|const FcMatrix * { $(FcValue*v)->u.m }|]
    (#const FcTypeCharSet) -> ValueCharSet . ConstPtr <$> [C.exp|const FcCharSet * { $(FcValue*v)->u.c }|]
    (#const FcTypeFTFace) -> ValueFace . ConstPtr <$> [C.exp|const FT_Face * { (const FT_Face*)($(FcValue*v)->u.f) }|]
    (#const FcTypeLangSet) -> ValueLangSet . ConstPtr <$> [C.exp|const FcLangSet * { $(FcValue*v)->u.l }|]
    (#const FcTypeRange) -> ValueRange . ConstPtr <$> [C.exp|const FcRange * { $(FcValue*v)->u.r }|]
    _ -> pure ValueUnknown

withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

withSelfMaybe :: Coercible a (Maybe (ForeignPtr a)) => a -> (Ptr a -> IO r) -> IO r
withSelfMaybe a f = maybe (f nullPtr) (`withForeignPtr` f) (coerce a)

withMaybeSelf :: Coercible a (ForeignPtr a) => Maybe a -> (Ptr a -> IO r) -> IO r
withMaybeSelf a f = maybe (f nullPtr) (`withSelf` f) a

instance Default Config where def = Config Nothing

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

-- * Inline C context

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

fontConfigCtx :: C.Context
fontConfigCtx = mempty
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
    , (C.TypeName "FT_Face", [t| Face |])
    , (C.TypeName "FcStrSet", [t| StrSet |])
    , (C.TypeName "FcValue", [t| Value |])
    , (C.TypeName "FcStrList", [t| StrList |])
    , (C.Struct "stat", [t| Stat |])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr",        anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char (Just C.Unsigned)))) [t| CUChar |] [| withCUString |])
    , ("str",         anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char Nothing))) [t| CChar |] [| withCString |])
    , ("cache",       anti (ptr (C.TypeName "FcCache")) [t| Cache|] [| withSelf |])
    , ("config",      anti (ptr (C.TypeName "FcConfig")) [t| Config |] [| withSelfMaybe |])
    , ("fontset",     anti (ptr (C.TypeName "FcFontSet")) [t| FontSet |] [| withSelf |])
    , ("objectset",   anti (ptr (C.TypeName "FcObjectSet")) [t| ObjectSet |] [| withSelf |])
    , ("charset",     anti (ptr (C.TypeName "FcCharSet")) [t| CharSet |] [| withSelf |])
    , ("langset",     anti (ptr (C.TypeName "FcLangSet")) [t| LangSet |] [| withSelf |])
    , ("strset",      anti (ptr (C.TypeName "FcStrSet")) [t| StrSet |] [| withSelf |])
    , ("pattern",     anti (ptr (C.TypeName "FcPattern")) [t| Pattern |] [| withSelf |])
    , ("matrix",      anti (ptr (C.TypeName "FcMatrix")) [t| Matrix |] [| withSelf |])
    , ("face",        anti (ptr (C.TypeName "FT_Face")) [t| Face |] [| withSelf |])
    , ("range",       anti (ptr (C.TypeName "FcRange")) [t| Range |] [| withSelf |])
    , ("value",       anti (ptr (C.TypeName "FcValue")) [t| Value |] [| with |])
    , ("stat",        anti (ptr (C.Struct "stat")) [t| Stat |] [| withSelf |])
    , ("maybe-stat",  anti (ptr (C.Struct "stat")) [t| Stat |] [| withMaybeSelf |])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
