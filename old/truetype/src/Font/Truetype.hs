{-# language Unsafe #-} -- this whole library makes a ton of assumptions the ttf is well-formed. This is a huge attack surface.
module Font.Truetype 
  ( 
  -- * Raw Fonts and Font Collections
    numberOfFonts
  , fontOffsetForIndex
  , findMatchingFont

  -- * Font
  , Font
  , font

  -- * Metadata
  , fontName

  -- * Font metrics
  , scaleForPixelHeight
  , scaleForMappingEmToPixels
  , fontBox
  , VMetrics(..), HasVMetrics(..)
  , fontVMetrics
  , fontVMetricsOS2

  -- * codepoints -> glyphs
  , glyphIndex

  -- * shapes
  , Move(..)
  , Shape(..)
  , glyphShape
  , codepointShape

  -- * Glyph operations
  , isGlyphEmpty

  -- * Glyph metrics 
  , glyphBox
  , glyphHMetrics
  , glyphKernAdvance

  -- * Codepoint metrics
  , codepointBox
  , codepointHMetrics
  , codepointKernAdvance

  -- * bitmaps
  --
  , glyphBitmap
  , glyphBitmapSubpixel
  , glyphBitmapBox
  , glyphBitmapBoxSubpixel
  , glyphMakeBitmap
  , glyphMakeBitmapSubpixel
  , glyphMakeBitmapSubpixelPrefilter
  , glyphSDF
  , codepointSDF

  , codepointBitmap
  , codepointBitmapSubpixel
  , codepointBitmapBox
  , codepointBitmapBoxSubpixel
  , codepointMakeBitmap
  , codepointMakeBitmapSubpixel
  , codepointMakeBitmapSubpixelPrefilter


  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString as Strict
import Data.byteString.Internal as Strict
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import Codex.Picture

import Font.Truetype.Raw



--------------------------------------------------------------------------------
-- * Raw Fonts and Font Collections
--------------------------------------------------------------------------------

-- This determines if the bytestring represents a font file.
-- @.ttc@ files may contain multiple fonts, while a @.ttf@ only
-- contains one. Returns -1 on error
numberOfFonts :: String.ByteString -> Int
numberOfFonts = fromIntegral $ unsafeDupablePerformIO $ unsafeUseAsCString bs c_getNumberOfFonts
{-# inline numberOfFonts #-}

fontOffsetForIndex :: Strict.ByteString -> Int -> Int
fontOffsetForIndex bs i = fromIntegral $ unsafeDupablePerformIO $ unsafeUseAsCString bs $ \c -> c_fontOffSetForIndex c (fromIntegral i)
{-# inline fontOffsetForIndex #-}

--------------------------------------------------------------------------------
-- * FontInfo
--------------------------------------------------------------------------------

-- | Creates a self contained FontInfo structure that contains the bytestring and that is managed by a ForeignPtr
font :: Strict.ByteString -> FontInfo
font (FS fp o l) = unsafePerformIO $ do
  fbuf <- mallocForeignPtrBytes (sizeOfFontInfo+l+1)
  withForeignPtr fbuf $ \buf -> do
     withForeignPtr fp $ \p -> memcpy (buf `plusPtr` sizeOfFontInfo) (p `plusPtr` o) (fromIntegral l)
     pokeByteOff buf (sizeOfFontInfo + l) (0::Word8)
     c_initFont buf (buf `plusPtr` sizeOfFontInfo)
  pure $ FontInfo fbuf 

--------------------------------------------------------------------------------
-- * Metrics
--------------------------------------------------------------------------------

-- | @'scaleForPixelHeight' font pixels@ Computes a scale factor to produce a font whose height is @pixels@ tall.
--
-- Height is measured as distance from the highest ascender to the lowest descender.
--
-- @
-- scale = pixels / (ascent - descent)
-- @
scaleForPixelHeight :: FontInfo -> Float -> Float
scaleForPixelHeight fp pixels = unsafeDupablePerformIO $ withFontInfo fp $ \p -> c_scaleForPixelHeight p pixels
{-# inline scaleForPixelHeight #-}

-- | Return the scale factor to prodiuce a font whose Em size is mapped to 'pixels' tall. 
scaleForMappingEmToPixels :: FontInfo -> Float -> Float
scaleForMappingEmToPixels fp pixels = unsafeDupablePerformIO $ withFontInfo fp $ \p -> c_scaleForMappingEmToPixels p pixels
{-# inline scleForMappingEmToPixels #-}

data VMetrics = VMetrics
  { _ascent, _descent, _lineGap :: {-# unpack #-} !Int
  } deriving (Eq,Ord,Show,Read)

makeClassy '' VMetrics

sizeOfCInt :: Int
sizeOfCInt = sizeOf (undefined :: CInt)
{-# inline sizeOfCInt #-}

fontVMetrics :: FontInfo -> VMetrics
fontVMetrics fp =
  unsafePerformIO $ 
    allocaArray 3 $ \ ascentPtr ->
      withFontInfo fp $ \ p -> do
        let descentPtr = ascentPtr `plusPtr` sizeOfCInt
            lineGapPtr = descentPtr `plusPtr` sizeOfCInt
        c_getFontVMetrics p ascentPtr descentPtr lineGapPtr
        VMetrics <$> peek ascentPtr <*> peek descentPtr <*> peek lineGapPtr

-- | Get "typographic" vertical font metrics from the OS/2 table (specific to MS/Windows TTF files).
fontVMetricsOS2 :: FontInfo -> Maybe VMetrics
fontVMetricsOS2 fp = 
  unsafePerformIO $ 
    allocaArray 3 $ \ascentPtr  ->
      withFontInfo fp $ \ p -> do
        let descentPtr = ascentPtr `plusPtr` sizeOfCInt
            lineGapPtr = descentPtr `plusPtr` sizeOfCInt
        b <- c_getFontVMetricsOS2 p ascentPtr descentPtr lineGapPtr
        if b == 0
        then pure Nothing
        else VMetrics <$> peek ascentPtr <*> peek descentPtr <*> peek lineGapPtr

fontBox :: FontInfo -> Box Identity
fontBox fp =
  unsafePerformIO $
    allocaArray 4 $ \(xminPtr :: Ptr CInt) ->
      withFontInfo fp $ \ p -> do
        let yminPtr = xminPtr `plusPtr` sizeOfCInt 
            xmaxPtr = yminPtr `plusPtr` sizeOfCInt
            ymaxPtr = xmaxPtr `plusPtr` sizeOfCInt 
        c_getFontBoundingBox p xminPtr yminPtr xmaxPtr ymaxPtr
        min <- Pt <$> peek xminPtr <*> peek yminPtr
        max <- Pt <$> peek xmaxPtr <*> peek ymaxPtr
        pure $ Box (Identity min) (max - min)


--------------------------------------------------------------------------------
-- * Glyph information
--------------------------------------------------------------------------------

isGlyphEmpty :: FontInfo -> Int -> Bool
isGlyphEmpty fp = 
  unsafeDupablePerformIO $
    withFontInfo fp $ \ p -> (0 /=) <$> c_isGlyphEmpty p (fromIntegral i)

-- | @'glyphIndex' font codepoint@ returns the index of the glyph representing that codepoint in the font, if any,
-- Returns 0 if the codepoint is not defined in this font.
glyphIndex :: FontInfo -> Int -> Int
glyphIndex fp cp = fromIntegral $ unsafeDupablePerformIO $ withFontInfo fp $ \p -> c_findGlyphIndex p (fromIntegral cp)
{-# inline glyphIndex #-}

glyphBox :: FontInfo -> Int -> Maybe (Box Identity)
glyphBox fp i = 
  unsafePerformIO $
    allocaArray 4 $ \(xminPtr :: Ptr CInt) ->
      withFontInfo fp $ \ p -> do
        let yminPtr = xminPtr `plusPtr` sizeOfCInt 
            xmaxPtr = yminPtr `plusPtr` sizeOfCInt
            ymaxPtr = xmaxPtr `plusPtr` sizeOfCInt 
        b <- c_getGlyphBox p i xminPtr yminPtr xmaxPtr ymaxPtr
        if b == 0
        then pure Nothing
        else do 
          min <- Pt <$> peek xminPtr <*> peek yminPtr
          max <- Pt <$> peek xmaxPtr <*> peek ymaxPtr
          pure $ Box (Identity min) (max - min)

codepointBox :: FontInfo -> Int -> Maybe (Box Identity)
codepointBox fp i =
  unsafePerformIO $
    allocaArray 4 $ \(xminPtr :: Ptr CInt) ->
      withFontInfo fp $ \ p -> do
        let yminPtr = xminPtr `plusPtr` sizeOfCInt 
            xmaxPtr = yminPtr `plusPtr` sizeOfCInt
            ymaxPtr = xmaxPtr `plusPtr` sizeOfCInt 
        b <- c_getCodepointBox p i xminPtr yminPtr xmaxPtr ymaxPtr
        if b == 0
        then pure Nothing
        else do 
          min <- Pt <$> peek xminPtr <*> peek yminPtr
          max <- Pt <$> peek xmaxPtr <*> peek ymaxPtr
          pure $ Box (Identity min) (max - min)

-- | 
-- @
-- (advanceWidth, leftSideBearing) = 'glyphHMetrics' font glyph
-- @
--
-- @leftSideBearing@ is the offset from the current horizontal position to the left edge of the glyph
-- @advanceWidth@ is the offset from the current horizontal position to the next horizontal position

glyphHMetrics :: FontInfo -> Int -> (Int, Int)
glyphHMetrics fp cp =
  unsafePerformIO $
    allocaArray 2 $ \ advanceWidthPtr ->
      withFontInfo fp $ \ p -> do
        let leftSideBearingPtr = advanceWidthPtr `plusPtr` sizeOfCInt
        c_getGlyphHMetrics p cp advanceWidthPtr leftSideBearingPtr
        advanceWidth <- peek advanceWidthPtr
        leftSideBearing <- peek leftSideBearingPtr
        pure (fromIntegral advanceWidth, fromIntegral leftSideBearing)


-- | 
-- @
-- (advanceWidth, leftSideBearing) = 'codepointHMetrics' font codepoint
-- @
--
-- @leftSideBearing@ is the offset from the current horizontal position to the left edge of the character
-- @advanceWidth@ is the offset from the current horizontal position to the next horizontal position

codepointHMetrics :: FontInfo -> Int -> (Int, Int)
codepointHMetrics fp cp =
  unsafePerformIO $
    allocaArray 2 $ \ advanceWidthPtr ->
      withFontInfo fp $ \ p -> do
        let leftSideBearingPtr = advanceWidthPtr `plusPtr` sizeOfCInt
        c_getCodePointHMetrics p cp advanceWidthPtr leftSideBearingPtr
        advanceWidth <- peek advanceWidthPtr
        leftSideBearing <- peek leftSideBearingPtr
        pure (fromIntegral advanceWidth, fromIntegral leftSideBearing)

-- | The amount to add to 'advance' between a given pair of glyphs for simple pairwise kerning.
glyphKernAdvance :: FontInfo -> Int -> Int -> Int
glyphKernAdvance fp g1 g2 =
  fromIntegral $ 
    unsafePerformIO $
      withFontInfo fp $ \p ->
        c_getGlyphKernAdvance p (fromIntegral g1) (fromIntegral g2)

-- | The amount to add to 'advance' between a given pair of codepoints for even simpler codepoint-based pairwise kerning.
--
-- You should probably be using harfbuzz.
codepointKernAdvance :: FontInfo -> Int -> Int -> Int
codepointKernAdvance fp cp1 cp2 =
  fromIntegral $ 
    unsafePerformIO $
      withFontInfo fp $ \p ->
        c_getCodepointKernAdvance p (fromIntegral cp1) (fromIntegral cp2)

data Move
  = MoveTo {-# unpack #-} !Pt
  | LineTo {-# unpack #-} !Pt
  | CurveTo {-# unpack #-} !Pt {-# unpack #-} !Pt
  | CubicTo {-# unpack #-} !Pt {-# unpack #-} !Pt {-# unpack #-} Pt

data Shape = Shape { shapeStart :: !Pt, shapeMoves :: [Move] }

glyphShape :: FontInfo -> Int -> Shape
codepointShape :: FontInfo -> Int -> Shape
