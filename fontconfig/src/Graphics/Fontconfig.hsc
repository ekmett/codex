{-# language ForeignFunctionInterface #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleContexts #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language DeriveAnyClass #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
{-# language CPP #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}

module Graphics.Fontconfig
  ( Config
  , init
  , initBringUptoDate
  , initLoadConfig
  , initLoadConfigAndFonts
  , initReinitialize
  , fini

  , getVersion

  , configAppFontAddDir
  , configAppFontAddFile
  , configAppFontClear
  , configBuildFonts
  , configCreate
  , configCurrent
  , configEnableHome
  , configGetCacheDirs
  , configGetConfigDirs
  , configGetConfigFiles
  , configGetFontDirs
  , configGetFonts
  , configHome
  , configParseAndLoad
  , configRescanInterval
  , configSysRoot
  , configUptoDate

  , ObjectSet
  , objectSet
  , objectSetAdd

  , Pattern
  , patternCreate
  , patternDuplicate
  , patternDuplicatePtr
  , patternEqual
  , patternEqualSubset
  , patternFilter
  , patternAddInteger, patternGetInteger
  , patternAddDouble,  patternGetDouble
  , patternAddBool,    patternGetBool
  , patternAddString,  patternGetString
  , patternAddCharSet, patternGetCharSet
  , patternAddLangSet, patternGetLangSet
  , patternAddRange,   patternGetRange
  , patternRemove
  , patternHash
  , patternObjectCount
  , patternDel
  , patternPrint
  , patternFormat

  , nameParse
  , nameUnparse

  , Range
  , rangeCreateDouble
  , rangeCreateInteger
  , rangeCopy
  , rangeCopyPtr
  , rangeGetDouble

  , FontSet
  , fontSet
  , fontSetAdd
  , fontSetList
  , fontSetPrint

  , Cache, Stat
  , dirCacheLoad
  , dirCacheLoadFile
  , dirCacheClean
  , dirCacheRescan
  , dirCacheUnlink
  , dirCacheRead
  , dirCacheCreateUUID
  , dirCacheDeleteUUID
  , dirCacheValid
  , cacheCreateTagFile
  , cacheDir
  , cacheCopySet
  , cacheSubdirs
  , cacheNumFont
  , pattern CACHE_SUFFIX
  , pattern DIR_CACHE_FILE
  , pattern USER_CACHE_FILE

  , StrSet
  , strSetCreate
  , strSetMember
  , strSetEqual
  , strSetAdd
  , strSetAddFilename
  , strSetDel

  , CharSet
  , charSetCreate
  , charSetAddChar
  , charSetDelChar
  , charSetCopy
  , charSetCopyPtr -- not const due to refcounting
  , charSetEqual
  , charSetIntersect
  , charSetUnion
  , charSetSubtract
  , charSetMerge
  , charSetHasChar
  , charSetCount
  , charSetIntersectCount
  , charSetSubtractCount
  , charSetIsSubset

  , LangSet
  , langSetCreate
  , langSetCopy
  , langSetCopyPtr
  , langSetAdd
  , langSetDel
  , langSetContains
  , langSetEqual
  , langSetHash
  , langSetUnion
  , langSetSubtract

  , defaultSubstitute
  , fontRenderPrepare

  , AllocationFailed(..)

  , Value
  , valueEqual
  , valueMatch
  , patternAdd
  , patternAddWeak
  , withStringValue
  , withBoolValue
  , withDoubleValue
  , withIntegerValue
  , withCharSetValue
  , withLangSetValue
  , withRangeValue
  , withMatrixValue


  , weightFromOpenTypeDouble
  , weightToOpenTypeDouble
  , weightFromOpenType
  , weightToOpenType

  , pattern FC_MAJOR
  , pattern FC_MINOR
  , pattern FC_REVISION
  , pattern FC_VERSION

  , pattern SLANT_ROMAN
  , pattern SLANT_ITALIC
  , pattern SLANT_OBLIQUE

  , pattern WIDTH_ULTRACONDENSED
  , pattern WIDTH_EXTRACONDENSED
  , pattern WIDTH_CONDENSED
  , pattern WIDTH_SEMICONDENSED
  , pattern WIDTH_NORMAL
  , pattern WIDTH_SEMIEXPANDED
  , pattern WIDTH_EXPANDED
  , pattern WIDTH_EXTRAEXPANDED
  , pattern WIDTH_ULTRAEXPANDED

  , pattern RGBA_UNKNOWN
  , pattern RGBA_RGB
  , pattern RGBA_BGR
  , pattern RGBA_VRGB
  , pattern RGBA_VBGR
  , pattern RGBA_NONE

  , pattern HINT_NONE
  , pattern HINT_SLIGHT
  , pattern HINT_MEDIUM
  , pattern HINT_FULL

  , pattern LCD_NONE
  , pattern LCD_DEFAULT
  , pattern LCD_LIGHT
  , pattern LCD_LEGACY

  , pattern WEIGHT_THIN
  , pattern WEIGHT_EXTRALIGHT
  , pattern WEIGHT_ULTRALIGHT
  , pattern WEIGHT_LIGHT
  , pattern WEIGHT_DEMILIGHT
  , pattern WEIGHT_SEMILIGHT
  , pattern WEIGHT_BOOK
  , pattern WEIGHT_REGULAR
  , pattern WEIGHT_NORMAL
  , pattern WEIGHT_MEDIUM
  , pattern WEIGHT_DEMIBOLD
  , pattern WEIGHT_SEMIBOLD
  , pattern WEIGHT_BOLD
  , pattern WEIGHT_EXTRABOLD
  , pattern WEIGHT_ULTRABOLD
  , pattern WEIGHT_BLACK
  , pattern WEIGHT_HEAVY
  , pattern WEIGHT_EXTRABLACK
  , pattern WEIGHT_ULTRABLACK

  -- * Standard properties
  , pattern ANTIALIAS
  , pattern ASPECT
  , pattern AUTOHINT
  , pattern CAPABILITY
  , pattern CHAR_WIDTH
  , pattern CHAR_HEIGHT
  , pattern CHARSET
  , pattern COLOR
  , pattern DECORATIVE
  , pattern DPI
  , pattern EMBEDDED_BITMAP
  , pattern EMBOLDEN
  , pattern FAMILY
  , pattern FAMILYLANG
  , pattern FILE
  , pattern FONTFORMAT
  , pattern FONTVERSION
  , pattern FONT_FEATURES
  , pattern FONT_VARIATIONS
  , pattern FOUNDRY
  , pattern FT_FACE
  , pattern FULLNAME
  , pattern FULLNAMELANG
  , pattern HASH
  , pattern HINTING
  , pattern HINT_STYLE
  , pattern INDEX
  , pattern LANG
  , pattern LCD_FILTER
  , pattern MATRIX
  , pattern MINSPACE
  , pattern NAMELANG
  , pattern OUTLINE
  , pattern PIXEL_SIZE
  , pattern POSTSCRIPT_NAME
  , pattern PRGNAME
  , pattern RASTERIZER
  , pattern RGBA
  , pattern SCALABLE
  , pattern SCALE
  , pattern SIZE
  , pattern SLANT
  , pattern SOURCE
  , pattern SPACING
  , pattern STYLE
  , pattern STYLELANG
  , pattern SYMBOL
  , pattern VARIABLE
  , pattern VERTICAL_LAYOUT
  , pattern WEIGHT
  , pattern WIDTH
  ) where

import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Coerce
import Data.Const.Unsafe
import Data.Foldable
import Data.Functor
import Data.StateVar
import Data.Traversable
import Data.Version
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import Prelude hiding (init)
import System.IO

import Graphics.Fontconfig.Internal
import Graphics.Fontconfig.Private

C.context $ C.baseCtx <> C.fptrCtx <> fontconfigCtx
C.include "<fontconfig/fontconfig.h>"

#include <fontconfig/fontconfig.h>

pattern CACHE_SUFFIX = (#const FC_CACHE_SUFFIX)
pattern DIR_CACHE_FILE = (#const FC_DIR_CACHE_FILE)
pattern USER_CACHE_FILE = (#const FC_USER_CACHE_FILE)



--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

nextDir :: Ptr StrList -> IO FilePath
nextDir xs = [C.exp| FcChar8 * { FcStrListNext($(FcStrList * xs)) }|] >>= \x -> if nullPtr /= x
  then peekCUString x
  else [] <$ [C.block| void { FcStrListDone($(FcStrList * xs)); }|]

consumeStrList :: Ptr StrList -> IO [FilePath]
consumeStrList xs = nextDir xs >>= \case
  [] -> return []
  x -> (x:) <$> consumeStrList xs

--------------------------------------------------------------------------------
-- * Config
--------------------------------------------------------------------------------

init :: MonadIO m => m Bool
init = liftIO $ [C.exp| int { FcInit() } |] <&> cbool

initBringUptoDate :: MonadIO m => m Bool
initBringUptoDate = liftIO $ [C.exp| int { FcInitBringUptoDate() }|] <&> cbool

initLoadConfig :: MonadIO m => m Config
initLoadConfig = liftIO $ [C.exp|FcConfig* { FcInitLoadConfig() }|] >>= foreignConfig

initLoadConfigAndFonts :: MonadIO m => m Config
initLoadConfigAndFonts = liftIO $ [C.exp|FcConfig* { FcInitLoadConfigAndFonts() }|] >>= foreignConfig

initReinitialize :: MonadIO m => m Bool
initReinitialize = liftIO $ [C.exp|int { FcInitReinitialize() }|] <&> cbool

getVersion :: MonadIO m => m Version
getVersion = liftIO $ [C.exp|int { FcGetVersion() }|] <&> \ (fromIntegral -> i) -> makeVersion [div i 10000, div i 100 `mod` 100, mod i 100]

fini :: MonadIO m => m ()
fini = liftIO [C.block|void { FcFini(); }|]

configCreate :: MonadIO m => m Config
configCreate = liftIO $ [C.exp|FcConfig* { FcConfigCreate() }|] >>= foreignConfig

configBuildFonts :: MonadIO m => Config -> m Bool
configBuildFonts cfg = liftIO $ [C.exp|int { FcConfigBuildFonts($config:cfg) }|] <&> cbool

configAppFontAddFile :: MonadIO m => Config -> FilePath -> m Bool
configAppFontAddFile cfg file = liftIO $ [C.exp|int { FcConfigAppFontAddFile($config:cfg,$ustr:file) }|] <&> cbool

configAppFontAddDir :: MonadIO m => Config -> FilePath -> m Bool
configAppFontAddDir cfg dir = liftIO $ [C.exp|int { FcConfigAppFontAddFile($config:cfg,$ustr:dir) }|] <&> cbool

configAppFontClear :: MonadIO m => Config -> m ()
configAppFontClear cfg = liftIO [C.block|void { FcConfigAppFontClear($config:cfg); }|]

-- configSubstitute :: MonadIO m => Config -> Pattern -> MatchKind -> m Bool
-- configSubstituteWithPat :: MonadIO m => Config -> Pattern -> Pattern -> MatchKind -> m Bool

configCurrent :: StateVar Config
configCurrent = StateVar g s where
  g = [C.exp|FcConfig* { FcConfigReference(0) }|] >>= foreignConfig
  s cfg = [C.block|void { FcConfigSetCurrent(FcConfigReference($config:cfg)); }|]

configHome :: MonadIO m => m FilePath
configHome = liftIO $
  [C.exp|const unsigned char * { FcConfigHome() }|] >>= \cstr -> peekCUString cstr <* free cstr

configEnableHome :: MonadIO m => Bool -> m Bool
configEnableHome (marshal -> enable) = liftIO $ [C.exp|int { FcConfigEnableHome($(int enable)) }|] <&> cbool

configGetFonts :: MonadIO m => Config -> SetName -> m FontSet
configGetFonts cfg (marshal -> n) = liftIO $
  [C.exp|FcFontSet * { FcConfigGetFonts($config:cfg,$(int n)) }|] >>= foreignFontSet

-- | an interval of 0 disables checks
configRescanInterval :: Config -> StateVar Int
configRescanInterval cfg = StateVar g s where
  g = [C.exp|int { FcConfigGetRescanInterval($config:cfg) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.exp|int { FcConfigSetRescanInterval($config:cfg,$(int v)) }|] >>= check . cbool

configUptoDate :: MonadIO m => Config -> m Bool
configUptoDate cfg = liftIO $ [C.exp|int { FcConfigUptoDate($config:cfg) }|] <&> cbool

-- | extract a list of the system font directories
configGetFontDirs :: MonadIO m => Config -> m [FilePath]
configGetFontDirs c = liftIO $ [C.exp|FcStrList * { FcConfigGetFontDirs($config:c) }|] >>= consumeStrList
{-# inlineable configGetFontDirs #-}

configGetConfigDirs :: MonadIO m => Config -> m [FilePath]
configGetConfigDirs c = liftIO $ [C.exp|FcStrList * { FcConfigGetConfigDirs($config:c) }|] >>= consumeStrList
{-# inlineable configGetConfigDirs #-}

configGetConfigFiles :: MonadIO m => Config -> m [FilePath]
configGetConfigFiles c = liftIO $ [C.exp|FcStrList * { FcConfigGetConfigFiles($config:c) }|] >>= consumeStrList
{-# inlineable configGetConfigFiles #-}

configGetCacheDirs :: MonadIO m => Config -> m [FilePath]
configGetCacheDirs c = liftIO $ [C.exp|FcStrList * { FcConfigGetCacheDirs($config:c) }|] >>= consumeStrList
{-# inlineable configGetCacheDirs #-}



-- |
-- @'get' ('configSysRoot' config)@ obtains the system root directory in @config@ if available.-
--
--
-- @'configSysRoot' config $= sysroot@ sets @sysroot@ as the system root directory.
-- @fontconfig@ prepends @sysroot@ to the cache directories in order to allow people to generate caches at build time.
--
-- Note that this changing the current config as a side-effect, as this function calls @FcConfigSetCurrent()@ internally.
configSysRoot :: Config -> StateVar FilePath
configSysRoot c = StateVar g s where
  g = [C.exp|const FcChar8 * { FcConfigGetSysRoot($config:c) }|] >>= peekCUString
  s v = [C.block|void { FcConfigSetSysRoot($config:c,$ustr:v); }|]

-- | @configParseAndLoad config file complain@ walks the configuration in 'file' and constructs the internal representation in 'config'.
--
-- Any include files referenced from within 'file' will be loaded and parsed. If 'complain' is False, no warning will be displayed if 'file'
-- does not exist. Error and warning messages will be output to stderr. Returns False if some error occurred while loading the file, either
-- a parse error, semantic error or allocation failure. Otherwise returns True.
configParseAndLoad :: MonadIO m => Config -> FilePath -> Bool -> m Bool
configParseAndLoad config file (marshal -> complain) = liftIO $ [C.exp|int { FcConfigParseAndLoad($config:config, $ustr:file, $(int complain)) }|] <&> cbool

--------------------------------------------------------------------------------
-- * Object Sets
--------------------------------------------------------------------------------

-- | Construct a fresh object set
objectSet :: MonadIO m => [String] -> m ObjectSet
objectSet xs = liftIO $ do
  o <- [C.exp|FcObjectSet * { FcObjectSetCreate() }|]
  for_ xs $ \x -> [C.exp|int { FcObjectSetAdd($(FcObjectSet * o),$str:x) }|] >>= check . cbool
  foreignObjectSet o
{-# inlinable objectSet #-}

-- | Add a string to an object set
objectSetAdd :: MonadIO m => ObjectSet -> String -> m ()
objectSetAdd os s = liftIO $ [C.exp|int { FcObjectSetAdd($objectset:os,$str:s) }|] >>= check . cbool
{-# inlinable objectSetAdd #-}

--------------------------------------------------------------------------------
-- * Patterns
--------------------------------------------------------------------------------

-- | @'patternCreate'@ creates a pattern with no properties; used to build patterns from scratch.
patternCreate :: MonadIO m => m Pattern
patternCreate = liftIO $ [C.exp|FcPattern * { FcPatternCreate() }|] >>= foreignPattern
{-# inlinable patternCreate #-}

-- | @'patternDuplicate' p@ copies a pattern, returning a new pattern that matches @p@. Each pattern may be modified without affecting the other.
patternDuplicate :: MonadIO m => Pattern -> m Pattern
patternDuplicate p = liftIO $ [C.exp|FcPattern * { FcPatternDuplicate($pattern:p) }|] >>= foreignPattern
{-# inlinable patternDuplicate #-}

-- | @'patternDuplicate' p@ copies a pattern, returning a new pattern that matches @p@. Each pattern may be modified without affecting the other.
--
-- This can be used when given a temporary reference to a foreign FcPattern* that isn't being managed by haskell
patternDuplicatePtr :: MonadIO m => Ptr Pattern -> m Pattern
patternDuplicatePtr p = liftIO $ [C.exp|FcPattern * { FcPatternDuplicate($(FcPattern * p)) }|] >>= foreignPattern
{-# inlinable patternDuplicatePtr #-}

-- | Produces a pattern from standard text format.
nameParse :: MonadIO m => String -> m Pattern
nameParse s = liftIO $ [C.exp|FcPattern * { FcNameParse($ustr:s) }|] >>= foreignPattern
{-# inlinable nameParse #-}

-- | Converts a given pattern into standard text format.
nameUnparse :: MonadIO m => Pattern -> m String
nameUnparse p = liftIO $ do
  cstr <- [C.exp|FcChar8 * { FcNameUnparse($pattern:p) }|]
  peekCUString cstr <* free cstr -- the return value is not static, but instead refers to newly allocated memory to be freed by the caller, per docs
{-# inlinable nameUnparse #-}

-- | Prints an easily readable version of the pattern to stdout. There is no provision for reparsing data in this format, it's just for diagnostics and debugging.
patternPrint :: MonadIO m => Pattern -> m ()
patternPrint p = liftIO $ do
  [C.block|void { FcPatternPrint($pattern:p); }|]
  hFlush stdout
{-# inlinable patternPrint #-}

-- | Returns Nothing if the format is invalid
--
-- <https://www.freedesktop.org/software/fontconfig/fontconfig-devel/fcpatternformat.html>
patternFormat :: MonadIO m => Pattern -> String -> m (Maybe String)
patternFormat p fmt = liftIO $ do
  cstr <- [C.exp|FcChar8* { FcPatternFormat($pattern:p, $ustr:fmt) }|]
  if cstr == nullPtr then return Nothing else Just <$> (peekCUString cstr <* free cstr)

-- | @'patternEqual' p q@ returns whether @p@ and @q@ are exactly alike.
patternEqual :: MonadIO m => Pattern -> Pattern -> m Bool
patternEqual p q = liftIO $ [C.exp|int { FcPatternEqual($pattern:p,$pattern:q) }|] <&> cbool
{-# inlinable patternEqual #-}

-- | check to see if the patterns are equivalent on a given object set
patternEqualSubset :: MonadIO m => Pattern -> Pattern -> ObjectSet -> m Bool
patternEqualSubset p q o = liftIO $ [C.exp|int { FcPatternEqualSubset($pattern:p,$pattern:q,$objectset:o) }|] <&> cbool
{-# inlinable patternEqualSubset #-}

-- | filter a pattern by an object set
patternFilter :: MonadIO m => Pattern -> ObjectSet -> m Pattern
patternFilter p o = liftIO $ [C.exp|FcPattern * { FcPatternFilter($pattern:p,$objectset:o) }|] >>= foreignPattern
{-# inlinable patternFilter #-}

--------------------------------------------------------------------------------
--- * Properties
--------------------------------------------------------------------------------

-- $todo
-- ftface          FT_Face Use the specified FreeType face object
-- charset         CharSet Unicode chars encoded by the font

-- |
-- @
-- Property        Description
-- ----------------------------------------------------------------
-- slant           Italic, oblique or roman
-- weight          Light, medium, demibold, bold or black
-- width           Condensed, normal or expanded
-- spacing         Proportional, dual-width, monospace or charcell
-- hintstyle       Automatic hinting style
-- index           The index of the font within the file
-- rgba            unknown, rgb, bgr, vrgb, vbgr,
--                 none - subpixel geometry
-- lcdfilter       Type of LCD filter
-- fontversion     Version number of the font
-- @
patternAddInteger :: MonadIO m => Pattern -> String -> Int -> m Bool
patternAddInteger p k (fromIntegral -> v) = liftIO $ [C.exp|int { FcPatternAddInteger($pattern:p,$str:k,$(int v)) }|] <&> cbool
{-# inlinable patternAddInteger #-}

patternGetInteger :: MonadIO m => Pattern -> String -> Int -> m (Result Int)
patternGetInteger p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetInteger($pattern:p,$str:k,$(int i),$(int * t)) }|]
    getResult result $ fromIntegral <$> peek t
{-# inlinable patternGetInteger #-}

-- |
-- @
-- Property        Description
-- ----------------------------------------------------------------
-- size            Point size
-- aspect          Stretches glyphs horizontally before hinting
-- pixelsize       Pixel size
-- scale           Scale factor for point->pixel conversions (deprecated)
-- dpi             Target dots per inch
-- @
patternAddDouble :: MonadIO m => Pattern -> String -> Double -> m Bool
patternAddDouble p k (coerce -> v) = liftIO $ [C.exp|int { FcPatternAddDouble($pattern:p,$str:k,$(double v)) }|] <&> cbool
{-# inline patternAddDouble #-}

patternGetDouble :: MonadIO m => Pattern -> String -> Int -> m (Result Double)
patternGetDouble p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetDouble($pattern:p,$str:k,$(int i),$(double * t)) }|]
    getResult result $ coerce <$> peek t
{-# inline patternGetDouble #-}

-- |
-- @
-- Property        Description
-- ----------------------------------------------------------------
-- antialias       Whether glyphs can be antialiased
-- hinting         Whether the rasterizer should use hinting
-- verticallayout  Use vertical layout
-- autohint        Use autohinter instead of normal hinter
-- globaladvance   Use font global advance data (deprecated)
-- outline         Whether the glyphs are outlines
-- scalable        Whether glyphs can be scaled
-- color           Whether any glyphs have color
-- minspace        Eliminate leading from line spacing
-- embolden        Rasterizer should synthetically embolden the font
-- embeddedbitmap  Use the embedded bitmap instead of the outline
-- decorative      Whether the style is a decorative variant
-- @
patternAddBool :: MonadIO m => Pattern -> String -> FcBool -> m Bool
patternAddBool p k (marshal -> v) = liftIO $ [C.exp|int { FcPatternAddBool($pattern:p,$str:k,$(int v)) }|] <&> cbool
{-# inline patternAddBool #-}

patternGetBool :: MonadIO m => Pattern -> String -> Int -> m (Result FcBool)
patternGetBool p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetBool($pattern:p,$str:k,$(int i),$(int * t)) }|]
    getResult result $ peek t <&> unmarshal'
{-# inline patternGetBool #-}

-- |
-- @
-- Property        Type    Description
-- --------------------------------------------------------------
-- family          String  Font family names
-- familylang      String  Languages corresponding to each family
-- style           String  Font style. Overrides weight and slant
-- stylelang       String  Languages corresponding to each style
-- fullname        String  Font full names (often includes style)
-- fullnamelang    String  Languages corresponding to each fullname
-- foundry         String  Font foundry name
-- file            String  The filename holding the font
-- rasterizer      String  Which rasterizer is in use (deprecated)
-- lang            String  List of RFC-3066-style languages this
--                         font supports
-- capability      String  List of layout capabilities in the font
-- fontformat      String  String name of the font format
-- fontfeatures    String  List of the feature tags in OpenType to be enabled
-- namelang        String  Language name to be used for the default value of
--                         familylang, stylelang, and fullnamelang
-- prgname         String  String  Name of the running program
-- postscriptname  String  Font family name in PostScript
-- @
patternAddString :: MonadIO m => Pattern -> String -> String -> m Bool
patternAddString p k v = liftIO $ [C.exp|int { FcPatternAddString($pattern:p,$str:k,$ustr:v) }|] <&> cbool
{-# inlinable patternAddString #-}

patternGetString :: MonadIO m => Pattern -> String -> Int -> m (Result String)
patternGetString p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetString($pattern:p,$str:k,$(int i),$(unsigned char ** t)) }|]
    getResult result $ peek t >>= peekCUString -- FcPatternGet* doesn't copy, so that is on us
{-# inlinable patternGetString #-}

-- | Supplies default values for underspecified font patterns
--
-- * Patterns without a specified style or weight are set to Medium
-- * Patterns without a specified style or sland are set to Roman
-- * Patterns without a specified pixel size are given one computed from any specified point size (default 12), dpi (default 75) and scale (default 1)
defaultSubstitute :: MonadIO m => Pattern -> m ()
defaultSubstitute p = liftIO [C.block|void { FcDefaultSubstitute($pattern:p); }|]
{-# inlinable defaultSubstitute #-}

-- |
--
-- Property   Description
-- --------------------------------------------------------------
-- charset    Unicode chars encoded by the font
patternAddCharSet :: MonadIO m => Pattern -> String -> CharSet -> m Bool
patternAddCharSet p k v = liftIO $ [C.exp|int { FcPatternAddCharSet($pattern:p,$str:k,$charset:v) }|] <&> cbool
{-# inlinable patternAddCharSet #-}

patternGetCharSet :: MonadIO m => Pattern -> String -> Int -> m (Result CharSet)
patternGetCharSet p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetCharSet($pattern:p,$str:k,$(int i),$(FcCharSet ** t)) }|]
    getResult result $ do
      cs <- peek t
      [C.exp|FcCharSet* { FcCharSetCopy($(FcCharSet* cs)) }|] >>= foreignCharSet -- making a copy is on us
{-# inlinable patternGetCharSet #-}

patternAddLangSet :: MonadIO m => Pattern -> String -> LangSet -> m Bool
patternAddLangSet p k v = liftIO $ [C.exp|int { FcPatternAddLangSet($pattern:p,$str:k,$langset:v) }|] <&> cbool
{-# inlinable patternAddLangSet #-}

patternGetLangSet :: MonadIO m => Pattern -> String -> Int -> m (Result LangSet)
patternGetLangSet p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetLangSet($pattern:p,$str:k,$(int i),$(FcLangSet ** t)) }|]
    getResult result $ do
      cs <- peek t
      [C.exp|FcLangSet* { FcLangSetCopy($(FcLangSet* cs)) }|] >>= foreignLangSet -- making a copy is on us
{-# inlinable patternGetLangSet #-}

patternAddRange :: MonadIO m => Pattern -> String -> Range -> m Bool
patternAddRange p k v = liftIO $ [C.exp|int { FcPatternAddRange($pattern:p,$str:k,$range:v) }|] <&> cbool
{-# inlinable patternAddRange #-}

patternGetRange :: MonadIO m => Pattern -> String -> Int -> m (Result Range)
patternGetRange p k (fromIntegral -> i) = liftIO $
  alloca $ \t -> do
    result <- [C.exp|int { FcPatternGetRange($pattern:p,$str:k,$(int i),$(FcRange ** t)) }|]
    getResult result $ do
      cs <- peek t
      [C.exp|FcRange* { FcRangeCopy($(FcRange* cs)) }|] >>= foreignRange -- making a copy is on us
{-# inlinable patternGetRange #-}

patternRemove :: MonadIO m => Pattern -> String -> Int -> m Bool
patternRemove p k (fromIntegral -> i) = liftIO $ [C.exp|int { FcPatternRemove($pattern:p,$str:k,$(int i)) }|] <&> cbool
{-# inlinable patternRemove #-}

patternHash :: MonadIO m => Pattern -> m Int
patternHash p = liftIO $ [C.exp|int { FcPatternHash($pattern:p) }|] <&> fromIntegral
{-# inlinable patternHash #-}

-- | Returns the number of objects p has.
patternObjectCount :: MonadIO m => Pattern -> m Int
patternObjectCount p = liftIO $ [C.exp|int { FcPatternObjectCount($pattern:p) }|] <&> fromIntegral
{-# inlinable patternObjectCount #-}

-- | Deletes all values associated with the property `object', returning whether the property existed or not.
patternDel :: MonadIO m => Pattern -> String -> m Bool
patternDel p k = liftIO $ [C.exp|int { FcPatternDel($pattern:p, $str:k) }|] <&> cbool
{-# inlinable patternDel #-}

--------------------------------------------------------------------------------
-- * Ranges
--------------------------------------------------------------------------------

sizeOfDouble :: Int
sizeOfDouble = sizeOf (undefined :: Double)
{-# inline sizeOfDouble #-}

rangeCreateDouble :: MonadIO m => Double -> Double -> m Range
rangeCreateDouble (coerce -> lo) (coerce -> hi) = liftIO $ [C.exp|FcRange * { FcRangeCreateDouble($(double lo),$(double hi)) }|] >>= foreignRange
{-# inlinable rangeCreateDouble #-}

rangeCreateInteger :: MonadIO m => Int -> Int -> m Range
rangeCreateInteger (fromIntegral -> lo) (fromIntegral -> hi) = liftIO $ [C.exp|FcRange * { FcRangeCreateInteger($(int lo),$(int hi)) }|] >>= foreignRange
{-# inlinable rangeCreateInteger #-}

rangeCopy :: MonadIO m => Range -> m Range
rangeCopy r = liftIO $ [C.exp|FcRange * { FcRangeCopy($range:r) }|] >>= foreignRange
{-# inlinable rangeCopy #-}

rangeCopyPtr :: (APtr p, MonadIO m) => p Range -> m Range
rangeCopyPtr (unsafePtr -> r) = liftIO $ [C.exp|FcRange * { FcRangeCopy($(const FcRange * r)) }|] >>= foreignRange
{-# inlinable rangeCopyPtr #-}

rangeGetDouble :: MonadIO m => Range -> m (Maybe (Double, Double))
rangeGetDouble r = liftIO $
  allocaArray 2 $ \ (lo :: Ptr CDouble) -> do
    let hi = lo `plusPtr` sizeOfDouble :: Ptr CDouble
    b <- [C.exp|int { FcRangeGetDouble($range:r,$(double * lo),$(double * hi)) }|]
    if cbool b
    then do
      x <- peek lo
      y <- peek hi
      pure $ Just (coerce x, coerce y)
    else return Nothing
{-# inlinable rangeGetDouble #-}

--------------------------------------------------------------------------------
-- * Font sets
--------------------------------------------------------------------------------

fontSet :: MonadIO m => [Pattern] -> m FontSet
fontSet ps = liftIO $ do
  s <- [C.exp|FcFontSet * { FcFontSetCreate() }|]
  for_ ps $ \p ->
    [C.block|int {
      FcPattern *p = $pattern:p;
      FcPatternReference(p);
      return FcFontSetAdd($(FcFontSet* s),p);
    }|] >>= check . cbool
  foreignFontSet s
{-# inlineable fontSet #-}

-- | add a pattern to a font set
fontSetAdd :: MonadIO m => FontSet -> Pattern -> m ()
fontSetAdd s p = liftIO $
  [C.block|int {
    FcPattern *p = $pattern:p;
    FcPatternReference(p);
    return FcFontSetAdd($fontset:s,p);
  }|] >>= check . cbool
{-# inlinable fontSetAdd #-}

fontSetPrint :: MonadIO m => FontSet -> m ()
fontSetPrint s = liftIO $ do
  [C.block|void { FcFontSetPrint($fontset:s); }|]
  hFlush stdout

-- | Selects fonts matching pattern from sets, creates patterns from those fonts containing only the objects
-- in object_set and returns the set of unique such patterns. If config is NULL, the default configuration is checked to be up to date, and used.

fontSetList :: (MonadIO m, Foldable f) => Config -> f FontSet -> Pattern -> ObjectSet -> m FontSet
fontSetList c (toList -> xs) p o = liftIO $
  runContT (traverse (ContT . withForeignPtr . coerce) xs) $ \ys -> withArrayLen ys $ \ (fromIntegral -> nsets) sets->
    [C.exp|FcFontSet* { FcFontSetList($config:c,$(FcFontSet** sets),$(int nsets),$pattern:p,$objectset:o) }|] >>= foreignFontSet
{-# inlineable fontSetList #-}

--------------------------------------------------------------------------------
-- * Caches
--------------------------------------------------------------------------------

cacheCreateTagFile :: MonadIO m => Config -> m ()
cacheCreateTagFile c = liftIO [C.block|void { FcCacheCreateTagFile($config:c); }|]
{-# inlinable cacheCreateTagFile #-}

-- | @'dirCacheLoad' dir cfg@ loads the cache for the given @dir@ if it exists, returns the cache and its filepath
dirCacheLoad :: MonadIO m => FilePath -> Config -> m (Maybe (Cache, FilePath))
dirCacheLoad dir cfg = liftIO $ -- withCString fdir $ \dir ->
  alloca $ \ ppath -> do
   cache <- [C.exp|FcCache * { FcDirCacheLoad($ustr:dir,$config:cfg,$(FcChar8** ppath)) }|]
   if cache == nullPtr then return Nothing else do
     path <- peek ppath >>= \x -> peekCUString x <* free x
     fcache <- foreignCache cache
     pure $ Just (fcache, path)

-- | This function loads a directory cache from cache_file and also returns the results of @stat@ on the cache file.
dirCacheLoadFile :: MonadIO m => FilePath -> Maybe Stat -> m Cache
dirCacheLoadFile cache_file s = liftIO $ [C.exp|FcCache * { FcDirCacheLoadFile($ustr:cache_file,$maybe-stat:s) }|] >>= foreignCache
{-# inlineable dirCacheLoadFile #-}

-- | @'dirCacheClean' cache_dir verbose@ tries to clean up the cache directory @cache_dir@. Returns 'True' if successful.
dirCacheClean :: MonadIO m => FilePath -> Bool -> m Bool
dirCacheClean dir (marshal -> v) = liftIO $ [C.exp|int { FcDirCacheClean($ustr:dir,$(int v)) }|] <&> cbool
{-# inlineable dirCacheClean #-}

dirCacheRescan :: MonadIO m => FilePath -> Config -> m (Maybe Cache)
dirCacheRescan dir c = liftIO $ do
  cache <- [C.exp|FcCache* { FcDirCacheRescan($ustr:dir,$config:c) }|]
  if cache == nullPtr then return Nothing else Just <$> foreignCache cache
{-# inline dirCacheRescan #-}

dirCacheUnlink :: MonadIO m => FilePath -> Config -> m Bool
dirCacheUnlink dir c = liftIO [C.exp|int { FcDirCacheUnlink($ustr:dir,$config:c) }|] <&> cbool
{-# inlinable dirCacheUnlink #-}

-- | @'dirCacheRead' dir force cfg@ will return a cache for @dir@. If force is false a valid cache file will be used if possible.
dirCacheRead :: MonadIO m => FilePath -> Bool -> Config -> m Cache
dirCacheRead dir (marshal -> force) cfg = liftIO $ [C.exp|FcCache* { FcDirCacheRead($ustr:dir,$(int force),$config:cfg) }|] >>= foreignCache
{-# inlinable dirCacheRead #-}

dirCacheCreateUUID :: MonadIO m => FilePath -> Bool -> Config -> m Bool
dirCacheCreateUUID dir (marshal -> force) cfg = liftIO $ [C.exp|int { FcDirCacheCreateUUID($ustr:dir,$(int force), $config:cfg) }|] <&> cbool
{-# inlinable dirCacheCreateUUID #-}

dirCacheDeleteUUID :: MonadIO m => FilePath -> Config -> m Bool
dirCacheDeleteUUID dir cfg = liftIO $ [C.exp|int { FcDirCacheDeleteUUID($ustr:dir,$config:cfg) }|] <&> cbool
{-# inlinable dirCacheDeleteUUID #-}

dirCacheValid :: MonadIO m => FilePath -> m Bool
dirCacheValid dir = liftIO $ [C.exp|int { FcDirCacheValid($ustr:dir) }|] <&> cbool
{-# inlinable dirCacheValid #-}

-- | @'cacheDir' cache@ returns the directory being cached here.
cacheDir :: MonadIO m => Cache -> m FilePath
cacheDir c = liftIO $ [C.exp|const FcChar8 * { FcCacheDir($cache:c) }|] >>= peekCUString -- manual dup'ing fits internal behavior of fontconfig
{-# inlinable cacheDir #-}

-- | @'cacheCopySet' cache@ returns the set of all fonts in the cache
cacheCopySet :: MonadIO m => Cache -> m FontSet
cacheCopySet c = liftIO $ [C.exp|FcFontSet* { FcCacheCopySet($cache:c) }|] >>= foreignFontSet
{-# inlinable cacheCopySet #-}

-- | @'cacheSubDirs' cache@ returns a list of the sub-directories contained in this cache
cacheSubdirs :: MonadIO m => Cache -> m [FilePath]
cacheSubdirs (Cache fc) = liftIO $ withForeignPtr fc $ \c -> do
  n <- [C.exp|int { FcCacheNumSubdir($(FcCache * c)) }|]
  for [0..n-1] $ \i ->
    [C.exp|const FcChar8 * { FcCacheSubdir($(FcCache * c),$(int i)) }|] >>= peekCUString -- not freeing matches observed internal fontconfig behavior
{-# inlinable cacheSubdirs #-}

-- | @'cacheNumFont' cache@ returns the number of fonts described by this cache.
cacheNumFont :: MonadIO m => Cache -> m Int
cacheNumFont c = liftIO $ [C.exp|int { FcCacheNumFont($cache:c) }|] <&> fromIntegral
{-# inlinable cacheNumFont #-}

-- | Create an empty set.
strSetCreate :: MonadIO m => m StrSet
strSetCreate = liftIO $ [C.exp|FcStrSet* { FcStrSetCreate() }|] >>= foreignStrSet

-- | @'strSetMember' set s@ returns whether @s@ is a member of @set@.
strSetMember :: MonadIO m => StrSet -> String -> m Bool
strSetMember set s = liftIO $ [C.exp|int { FcStrSetMember($strset:set,$ustr:s) }|] <&> cbool

-- | @'strSetEqual' set_a set_b@ returns whether @set_a@ contains precisely the same strings as @set_b@.
-- Ordering of strings within the two sets is not considered.
strSetEqual :: MonadIO m => StrSet -> StrSet -> m Bool
strSetEqual s t = liftIO $ [C.exp|int { FcStrSetEqual($strset:s,$strset:t) }|] <&> cbool

strSetAdd :: MonadIO m => StrSet -> String -> m Bool
strSetAdd set s = liftIO $ [C.exp|int { FcStrSetAdd($strset:set,$ustr:s) }|] <&> cbool

strSetAddFilename :: MonadIO m => StrSet -> FilePath -> m Bool
strSetAddFilename  set s = liftIO $ [C.exp|int { FcStrSetAddFilename($strset:set,$ustr:s) }|] <&> cbool

strSetDel :: MonadIO m => StrSet -> String -> m Bool
strSetDel set s = liftIO $ [C.exp|int { FcStrSetDel ($strset:set,$ustr:s) }|] <&> cbool

-- * Char Sets

char32 :: Char -> CInt
char32 = fromIntegral . fromEnum

charSetCreate :: MonadIO m => m CharSet
charSetCreate = liftIO $ [C.exp|FcCharSet* { FcCharSetCreate() }|] >>= foreignCharSet
{-# inlinable charSetCreate #-}

charSetAddChar :: MonadIO m => CharSet -> Char -> m ()
charSetAddChar c (char32 -> i) = liftIO [C.block|void { FcCharSetAddChar($charset:c,$(int i)); }|]
{-# inlinable charSetAddChar #-}

charSetDelChar :: MonadIO m => CharSet -> Char -> m ()
charSetDelChar c (char32 -> i) = liftIO [C.block|void { FcCharSetDelChar($charset:c,$(int i)); }|]
{-# inlinable charSetDelChar #-}

charSetCopy :: MonadIO m => CharSet -> m CharSet
charSetCopy c = liftIO $ [C.exp|FcCharSet * { FcCharSetCopy($charset:c) }|] >>= foreignCharSet
{-# inlinable charSetCopy #-}

charSetCopyPtr :: (APtr p, MonadIO m) => p CharSet -> m CharSet
charSetCopyPtr (unsafePtr -> c) = liftIO $ [C.exp|FcCharSet * { FcCharSetCopy($(FcCharSet*c)) }|] >>= foreignCharSet
{-# inlinable charSetCopyPtr #-}

charSetEqual :: MonadIO m => CharSet -> CharSet -> m Bool
charSetEqual c d = liftIO $ [C.exp|int { FcCharSetEqual($charset:c,$charset:d) }|] <&> cbool
{-# inlinable charSetEqual #-}

charSetIntersect :: MonadIO m => CharSet -> CharSet -> m CharSet
charSetIntersect c d = liftIO $ [C.exp|FcCharSet * { FcCharSetIntersect($charset:c, $charset:d) }|] >>= foreignCharSet
{-# inlinable charSetIntersect #-}

charSetUnion :: MonadIO m => CharSet -> CharSet -> m CharSet
charSetUnion c d = liftIO $ [C.exp|FcCharSet * { FcCharSetUnion($charset:c, $charset:d) }|] >>= foreignCharSet
{-# inlinable charSetUnion #-}

charSetSubtract :: MonadIO m => CharSet -> CharSet -> m CharSet
charSetSubtract c d = liftIO $ [C.exp|FcCharSet * { FcCharSetSubtract($charset:c, $charset:d) }|] >>= foreignCharSet
{-# inlinable charSetSubtract #-}

charSetMerge :: MonadIO m => CharSet -> CharSet -> m Bool
charSetMerge c d = liftIO $ [C.exp|int { FcCharSetEqual($charset:c,$charset:d) }|] <&> cbool
{-# inlinable charSetMerge #-}

charSetHasChar :: MonadIO m => CharSet -> Char -> m Bool
charSetHasChar c (char32 -> i) = liftIO $ [C.exp|int { FcCharSetHasChar($charset:c,$(int i)) }|] <&> cbool
{-# inlinable charSetHasChar #-}

charSetCount :: MonadIO m => CharSet -> m Int
charSetCount c = liftIO $ [C.exp|int { FcCharSetCount($charset:c) }|] <&> fromIntegral
{-# inlinable charSetCount #-}

charSetIntersectCount :: MonadIO m => CharSet -> CharSet -> m Int
charSetIntersectCount c d = liftIO $ [C.exp|int { FcCharSetIntersectCount($charset:c, $charset:d) }|] <&> fromIntegral
{-# inlinable charSetIntersectCount #-}

charSetSubtractCount :: MonadIO m => CharSet -> CharSet -> m Int
charSetSubtractCount c d = liftIO $ [C.exp|int { FcCharSetSubtractCount($charset:c, $charset:d) }|] <&> fromIntegral
{-# inlinable charSetSubtractCount #-}

charSetIsSubset :: MonadIO m => CharSet -> CharSet -> m Bool
charSetIsSubset c d = liftIO $ [C.exp|int { FcCharSetIsSubset($charset:c,$charset:d) }|] <&> cbool
{-# inlinable charSetIsSubset #-}

-- * Lang Sets

langSetCreate :: MonadIO m => m LangSet
langSetCreate = liftIO $ [C.exp|FcLangSet* { FcLangSetCreate() }|] >>= foreignLangSet
{-# inlinable langSetCreate #-}

langSetCopy :: MonadIO m => LangSet -> m LangSet
langSetCopy c = liftIO $ [C.exp|FcLangSet * { FcLangSetCopy($langset:c) }|] >>= foreignLangSet
{-# inlinable langSetCopy #-}

langSetCopyPtr :: (APtr p, MonadIO m) => p LangSet -> m LangSet
langSetCopyPtr (unsafePtr -> c) = liftIO $ [C.exp|FcLangSet * { FcLangSetCopy($(const FcLangSet * c)) }|] >>= foreignLangSet
{-# inlinable langSetCopyPtr #-}

langSetAdd :: MonadIO m => LangSet -> String -> m Bool
langSetAdd c s = liftIO $ [C.exp|int { FcLangSetAdd($langset:c,$ustr:s) }|] <&> cbool
{-# inlinable langSetAdd #-}

langSetDel :: MonadIO m => LangSet -> String -> m Bool
langSetDel c s = liftIO $ [C.exp|int { FcLangSetDel($langset:c,$ustr:s) }|] <&> cbool
{-# inlinable langSetDel #-}

langSetContains :: MonadIO m => LangSet -> LangSet -> m Bool
langSetContains c d = liftIO $ [C.exp|int { FcLangSetContains($langset:c,$langset:d) }|] <&> cbool
{-# inlinable langSetContains #-}

langSetEqual :: MonadIO m => LangSet -> LangSet -> m Bool
langSetEqual c d = liftIO $ [C.exp|int { FcLangSetEqual($langset:c,$langset:d) }|] <&> cbool
{-# inlinable langSetEqual #-}

langSetHash :: MonadIO m => LangSet -> m Int
langSetHash c = liftIO $ [C.exp|int { FcLangSetHash($langset:c) }|] <&> fromIntegral
{-# inlinable langSetHash #-}

langSetUnion :: MonadIO m => LangSet -> LangSet -> m LangSet
langSetUnion c d = liftIO $ [C.exp|FcLangSet * { FcLangSetUnion($langset:c, $langset:d) }|] >>= foreignLangSet
{-# inlinable langSetUnion #-}

langSetSubtract :: MonadIO m => LangSet -> LangSet -> m LangSet
langSetSubtract c d = liftIO $ [C.exp|FcLangSet * { FcLangSetSubtract($langset:c, $langset:d) }|] >>= foreignLangSet
{-# inlinable langSetSubtract #-}

withValue :: forall a r. Storable a => Type a -> a -> (Ptr Value -> IO r) -> IO r
withValue (Type type_) u f = allocaBytes (#size FcValue) $ \p -> do
  (#poke FcValue, type) p type_
  (#poke FcValue, u) p u
  f p

valueEqual :: MonadIO m => Ptr Value -> Ptr Value -> m Bool
valueEqual a b = liftIO $ [C.exp|int { FcValueEqual(*$(FcValue * a),*$(FcValue * b)) }|] <&> cbool
{-# inlinable valueEqual #-}

withStringValue :: String -> (Ptr Value -> IO r) -> IO r
withStringValue s f = withCUString s $ \p -> withValue TypeString (constant p) f

withBoolValue :: Bool -> (Ptr Value -> IO r) -> IO r
withBoolValue = withValue TypeBool . FcBool . boolc

withDoubleValue :: Double -> (Ptr Value -> IO r) -> IO r
withDoubleValue = withValue TypeDouble

withCharSetValue :: CharSet -> (Ptr Value -> IO r) -> IO r
withCharSetValue cs f = withForeignPtr (coerce cs) $ \p -> withValue TypeCharSet (constant p) f

withLangSetValue :: LangSet -> (Ptr Value -> IO r) -> IO r
withLangSetValue cs f = withForeignPtr (coerce cs) $ \p -> withValue TypeLangSet (constant p) f

withIntegerValue :: Int -> (Ptr Value -> IO r) -> IO r
withIntegerValue cs f = withValue TypeInteger (fromIntegral cs) f

withRangeValue :: Range -> (Ptr Value -> IO r) -> IO r
withRangeValue cs f = withForeignPtr (coerce cs) $ \p -> withValue TypeRange (constant p) f

withMatrixValue :: Matrix -> (Ptr Value -> IO r) -> IO r
withMatrixValue cs f = withForeignPtr (coerce cs) $ \p -> withValue TypeMatrix (constant p) f

patternAdd :: MonadIO m => Pattern -> String -> Ptr Value -> Bool -> m Bool
patternAdd p k v (marshal -> append) = liftIO $
  [C.exp|int { FcPatternAdd($pattern:p,$str:k,*($(FcValue * v)),$(int append)) }|] <&> cbool

patternAddWeak :: MonadIO m => Pattern -> String -> Ptr Value -> Bool -> m Bool
patternAddWeak p k v (marshal -> append) = liftIO $
  [C.exp|int { FcPatternAddWeak($pattern:p,$str:k,*($(FcValue *v)),$(int append)) }|] <&> cbool

valueMatch :: (MonadIO m, Storable a) => Type a -> Ptr Value -> m (Maybe a)
valueMatch (Type type_) p = liftIO $ do
  type2 <- (#peek FcValue, type) p
  if type_ == type2 then Just <$> (#peek FcValue, u) p else pure Nothing

-- TODO: patternGet?


pattern FC_MAJOR = (#const FC_VERSION) :: Int
pattern FC_MINOR = (#const FC_MINOR) :: Int
pattern FC_REVISION = (#const FC_REVISION) :: Int
pattern FC_VERSION = (#const FC_VERSION) :: Int

-- | @'fontRenderPrepare' cfg pat font@ creates a new pattern consisting of elements of @font@ not appearing in @pat@, elements of @pat@ not appearing in @font@ and the best matching value from @pat@ for elements appearing in both. The result is passed to @FcConfigSubstituteWithPat@ with @kind@ @FcMatchFont@ and then returned.
fontRenderPrepare :: MonadIO m => Config -> Pattern -> Pattern -> m Pattern
fontRenderPrepare cfg pat font = liftIO $ [C.exp|FcPattern * { FcFontRenderPrepare($config:cfg,$pattern:pat,$pattern:font) }|] >>= foreignPattern
{-# inlinable fontRenderPrepare #-}


pattern SLANT_ROMAN = (#const FC_SLANT_ROMAN)
pattern SLANT_ITALIC = (#const FC_SLANT_ITALIC)
pattern SLANT_OBLIQUE = (#const FC_SLANT_OBLIQUE)

pattern WIDTH_ULTRACONDENSED  = (#const FC_WIDTH_ULTRACONDENSED) :: Int
pattern WIDTH_EXTRACONDENSED  = (#const FC_WIDTH_EXTRACONDENSED) :: Int
pattern WIDTH_CONDENSED  = (#const FC_WIDTH_CONDENSED) :: Int
pattern WIDTH_SEMICONDENSED  = (#const FC_WIDTH_SEMICONDENSED) :: Int
pattern WIDTH_NORMAL   = (#const FC_WIDTH_NORMAL ) :: Int
pattern WIDTH_SEMIEXPANDED  = (#const FC_WIDTH_SEMIEXPANDED) :: Int
pattern WIDTH_EXPANDED  = (#const FC_WIDTH_EXPANDED) :: Int
pattern WIDTH_EXTRAEXPANDED  = (#const FC_WIDTH_EXTRAEXPANDED) :: Int
pattern WIDTH_ULTRAEXPANDED  = (#const FC_WIDTH_ULTRAEXPANDED) :: Int

pattern RGBA_UNKNOWN  = (#const FC_RGBA_UNKNOWN) :: Int
pattern RGBA_RGB  = (#const FC_RGBA_RGB) :: Int
pattern RGBA_BGR  = (#const FC_RGBA_BGR) :: Int
pattern RGBA_VRGB  = (#const FC_RGBA_VRGB) :: Int
pattern RGBA_VBGR  = (#const FC_RGBA_VBGR) :: Int
pattern RGBA_NONE  = (#const FC_RGBA_NONE) :: Int

pattern HINT_NONE = (#const FC_HINT_NONE) :: Int
pattern HINT_SLIGHT = (#const FC_HINT_SLIGHT) :: Int
pattern HINT_MEDIUM = (#const FC_HINT_MEDIUM) :: Int
pattern HINT_FULL = (#const FC_HINT_FULL) :: Int

pattern LCD_NONE  = (#const FC_LCD_NONE) :: Int
pattern LCD_DEFAULT  = (#const FC_LCD_DEFAULT) :: Int
pattern LCD_LIGHT  = (#const FC_LCD_LIGHT) :: Int
pattern LCD_LEGACY  = (#const FC_LCD_LEGACY) :: Int

pattern WEIGHT_THIN = (#const FC_WEIGHT_THIN) :: Int
pattern WEIGHT_EXTRALIGHT = (#const FC_WEIGHT_EXTRALIGHT) :: Int
pattern WEIGHT_ULTRALIGHT = (#const FC_WEIGHT_ULTRALIGHT) :: Int
pattern WEIGHT_LIGHT = (#const FC_WEIGHT_LIGHT) :: Int
pattern WEIGHT_DEMILIGHT = (#const FC_WEIGHT_DEMILIGHT) :: Int
pattern WEIGHT_SEMILIGHT = (#const FC_WEIGHT_SEMILIGHT) :: Int
pattern WEIGHT_BOOK = (#const FC_WEIGHT_BOOK) :: Int
pattern WEIGHT_REGULAR = (#const FC_WEIGHT_REGULAR) :: Int
pattern WEIGHT_NORMAL = (#const FC_WEIGHT_NORMAL) :: Int
pattern WEIGHT_MEDIUM = (#const FC_WEIGHT_MEDIUM) :: Int
pattern WEIGHT_DEMIBOLD = (#const FC_WEIGHT_DEMIBOLD) :: Int
pattern WEIGHT_SEMIBOLD = (#const FC_WEIGHT_SEMIBOLD) :: Int
pattern WEIGHT_BOLD = (#const FC_WEIGHT_BOLD) :: Int
pattern WEIGHT_EXTRABOLD = (#const FC_WEIGHT_EXTRABOLD) :: Int
pattern WEIGHT_ULTRABOLD = (#const FC_WEIGHT_ULTRABOLD) :: Int
pattern WEIGHT_BLACK = (#const FC_WEIGHT_BLACK) :: Int
pattern WEIGHT_HEAVY = (#const FC_WEIGHT_HEAVY) :: Int
pattern WEIGHT_EXTRABLACK = (#const FC_WEIGHT_EXTRABLACK) :: Int
pattern WEIGHT_ULTRABLACK = (#const FC_WEIGHT_ULTRABLACK) :: Int

weightFromOpenTypeDouble :: Double -> Double
weightFromOpenTypeDouble (coerce -> d) = coerce [C.pure|double { FcWeightFromOpenTypeDouble($(double d)) }|]

weightToOpenTypeDouble :: Double -> Double
weightToOpenTypeDouble (coerce -> d) = coerce [C.pure|double { FcWeightToOpenTypeDouble($(double d)) }|]

weightFromOpenType :: Int -> Int
weightFromOpenType (fromIntegral -> i) = fromIntegral [C.pure|int { FcWeightFromOpenType($(int i)) }|]

weightToOpenType :: Int -> Int
weightToOpenType (fromIntegral -> i) = fromIntegral [C.pure|int { FcWeightToOpenType($(int i)) }|]

-- well known properties
pattern FAMILY           = "family" -- String
pattern STYLE            = "style" -- String
pattern SLANT            = "slant" -- Int
pattern WEIGHT           = "weight" -- Int
pattern SIZE             = "size" -- Range (double)
pattern ASPECT           = "aspect" -- Double
pattern PIXEL_SIZE       = "pixelsize" -- Double
pattern SPACING          = "spacing" -- Int
pattern FOUNDRY          = "foundry" -- String
pattern ANTIALIAS        = "antialias" -- Bool (depends)
pattern HINTING          = "hinting" -- Bool (true)
pattern HINT_STYLE       = "hintstyle" -- Int
pattern VERTICAL_LAYOUT  = "verticallayout" -- Bool (false)
pattern AUTOHINT         = "autohint" -- Bool (false)
-- FC_GLOBAL_ADVANCE is deprecated. this is simply ignored on freetype 2.4.5 or later
-- pattern GLOBAL_ADVANCE   = "globaladvance" -- Bool (true)
pattern WIDTH            = "width" -- Int
pattern FILE             = "file" -- String
pattern INDEX            = "index" -- Int
pattern FT_FACE          = "ftface" -- FT_Face
pattern RASTERIZER       = "rasterizer" -- String (deprecated)
pattern OUTLINE          = "outline" -- Bool
pattern SCALABLE         = "scalable" -- Bool
pattern COLOR            = "color" -- Bool
pattern VARIABLE         = "variable" -- Bool
pattern SCALE            = "scale" -- double (deprecated)
pattern SYMBOL           = "symbol" -- Bool
pattern DPI              = "dpi" -- double
pattern RGBA             = "rgba" -- Int
pattern MINSPACE         = "minspace" -- Bool use minimum line spacing
pattern SOURCE           = "source" -- String (deprecated)
pattern CHARSET          = "charset" -- CharSet
pattern LANG             = "lang" -- String RFC 3066 langs
pattern FONTVERSION      = "fontversion" -- Int from 'head' table
pattern FULLNAME         = "fullname" -- String
pattern FAMILYLANG       = "familylang" -- String RFC 3066 langs
pattern STYLELANG        = "stylelang" -- String RFC 3066 langs
pattern FULLNAMELANG     = "fullnamelang" -- String RFC 3066 langs
pattern CAPABILITY       = "capability" -- String
pattern FONTFORMAT       = "fontformat" -- String
pattern EMBOLDEN         = "embolden" -- Bool - true if emboldening needed
pattern EMBEDDED_BITMAP  = "embeddedbitmap" -- Bool - true to enable embedded bitmaps
pattern DECORATIVE       = "decorative" -- Bool - true if style is a decorative variant
pattern LCD_FILTER       = "lcdfilter" -- Int
pattern FONT_FEATURES    = "fontfeatures" -- String
pattern FONT_VARIATIONS  = "fontvariations" -- String
pattern NAMELANG         = "namelang" -- String RFC 3866 langs
pattern PRGNAME          = "prgname" -- String
pattern HASH             = "hash" -- String (deprecated)
pattern POSTSCRIPT_NAME  = "postscriptname" -- String

pattern CHAR_WIDTH = "charwidth" -- Int
pattern CHAR_HEIGHT = "charheight" -- Int
pattern MATRIX = "matrix" -- Matrix

