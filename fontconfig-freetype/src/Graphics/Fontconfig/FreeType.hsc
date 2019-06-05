{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Fontconfig.FreeType
( freeTypeCharIndex
, freeTypeCharSet
, freeTypeCharSetAndSpacing
, freeTypeQuery
, freeTypeQueryAll
, freeTypeQueryFace
-- pattern matching
, pattern TypeFace
, withFaceValue
, matchFaceValue
, patternAddFace
, patternGetFace
) where

import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Data.Traversable (for)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Fontconfig
import Graphics.Fontconfig.Internal
import Graphics.Fontconfig.Private (withCUString)
import Graphics.FreeType.Internal (Face, FaceRec, foreignFace, freeTypeCtx)
import qualified Language.C.Inline as C

C.context $ C.baseCtx <> C.fptrCtx <> fontconfigCtx <> freeTypeCtx
C.include "<fontconfig/fontconfig.h>"
C.include "<fontconfig/fcfreetype.h>"
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"

#ifndef HLINT
#include <fontconfig/fontconfig.h>
#include <fontconfig/fcfreetype.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#endif

freeTypeCharIndex :: MonadIO m => Face -> Char -> m Int
freeTypeCharIndex f (fromIntegral . fromEnum -> c) = liftIO $ [C.exp|int { FcFreeTypeCharIndex($fptr-ptr:(FT_Face f),$(FcChar32 c)) }|] <&> fromIntegral

freeTypeCharSet :: MonadIO m => Face -> m CharSet
freeTypeCharSet f = liftIO $ [C.exp|FcCharSet * { FcFreeTypeCharSet($fptr-ptr:(FT_Face f),0) }|] >>= foreignCharSet

freeTypeCharSetAndSpacing :: MonadIO m => Face -> m (CharSet, Spacing)
freeTypeCharSetAndSpacing f = liftIO $ alloca $ \ip ->
  (,) <$> ([C.exp|FcCharSet * { FcFreeTypeCharSet($fptr-ptr:(FT_Face f),0) }|] >>= foreignCharSet) <*> peek ip
  
-- | Construct a pattern representing the nth face in the file. Returns the number of faces in the file as well.
freeTypeQuery :: MonadIO m => FilePath -> Int -> m (Pattern, Int)
freeTypeQuery p (fromIntegral -> i) = liftIO $ alloca $ \count -> 
  (,) <$> ([C.exp|FcPattern * { FcFreeTypeQuery($ustr:p,$(int i),0,$(int * count)) }|] >>= foreignPattern)
      <*> (peek count <&> fromIntegral)

-- | Constructs patterns found in 'file'. If the id is -1 then all patterns found in the file are added to the supplied set, otherwise
-- just the selected pattern is added. Returns the number of patterns added to the fontset and the number of faces in the file.
freeTypeQueryAll :: MonadIO m => FilePath -> Int -> FontSet -> m (Int, Int)
freeTypeQueryAll p (fromIntegral -> i) fs = liftIO $ alloca $ \count -> 
  (,) <$> ([C.exp|unsigned int { FcFreeTypeQueryAll($ustr:p,$(int i),0,$(int * count),$fontset:fs) }|] <&> fromIntegral)
      <*> (peek count <&> fromIntegral)

-- | Constructs a pattern representing a given font face. The FilePath and id are used soly as data for pattern elements. (FC_FILE, FC_INDEX, possibly FC_FAMILY).
freeTypeQueryFace :: MonadIO m => Face -> FilePath -> Int -> m Pattern
freeTypeQueryFace f p (fromIntegral -> i) = liftIO $ [C.exp|FcPattern * { FcFreeTypeQueryFace($fptr-ptr:(FT_Face f),$ustr:p,$(int i),0) }|] >>= foreignPattern

pattern TypeFace :: Type (Ptr FaceRec)
pattern TypeFace = #const FcTypeFTFace

withFaceValue :: Face -> (Ptr Value -> IO r) -> IO r
withFaceValue face f = withForeignPtr face $ \p -> withValue TypeFace p f

-- note this will be an immutable face you should not edit!
matchFaceValue :: MonadIO m => Ptr Value -> m (Maybe Face)
matchFaceValue v = liftIO $ do
    mf <- matchValue TypeFace v
    for mf $ \f -> do
      [C.block|void { FT_Reference_Face($(FT_Face f)); }|]
      foreignFace f

patternAddFace :: MonadIO m => Pattern -> String -> Face -> m Bool
patternAddFace p k v = liftIO $ [C.exp|int { FcPatternAddFTFace($pattern:p,$str:k,$fptr-ptr:(FT_Face v)) }|] <&> (/=0)
{-# inlinable patternAddFace #-}

patternGetFace :: MonadIO m => Pattern -> String -> Int -> m (Maybe Face)
patternGetFace p k (fromIntegral -> i) = liftIO $
  alloca $ \fp -> do
    result <- [C.exp|FcResult { FcPatternGetFTFace($pattern:p,$str:k,$(int i),$(FT_Face * fp)) }|]
    getResult result $ do
      f <- peek fp
      [C.block|void { FT_Reference_Face($(FT_Face f)); }|]
      foreignFace f
{-# inlinable patternGetFace #-}
