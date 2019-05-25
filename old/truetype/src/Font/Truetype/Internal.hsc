-- Graphics.Truetype.Internal
-- Graphics.Truetype.Font
-- Graphics.Truetype.Glyph
-- Graphics.Truetype.Codepoint
-- Graphics.Truetype.Shape
-- Graphics.Truetype.Bitmap
-- Graphics.Truetype.SDF

{-# language ForeignFunctionInterface #-}
module Font.Truetype.Raw -- Font.Info?
  ( Font(..)
  , withFont
  , sizeOfFont
  , c_getNumberOfFonts
  , c_getFontOffsetForIndex
  , c_initFont
  , c_findGlyphIndex
  , c_scaleForPixelHeight
  , c_scaleForMappingEmToPixels
  ) where

-- import Data.Atlas.Raw

#include "stb_rect_pack.h"
#include "stb_truetype.h"

-- * Raw font file manipulation

data Style = Style { _styleBold :: Bool, _styleItalic :: Bool, _styleUnderlined :: Bool }
makeClassy ''Style

instance Semigroup Style where
  Style a b c <> Style d e f = Style (a || d) (b || e) (c || f) 

instance Monoid Style where
  mempty = Style False False False
 
bold :: Style
bold = Style True False False

italic :: Style
italic = Style False True False

underlined :: Style
underlined = Style False False True

none :: Style
none = Style False False False

-- Nothing represents 'don't care'
style :: Maybe Style -> CInt
style Nothing = #const STBTT_MACSTYLE_DONTCARE
style (Just (Style b i u))
  | t == 0 = #const STBTT_MACSTYLE_NONE
  | otherwise = fromIntegral t
  where t = (#const STBTT_MACSTYLE_BOLD) * fromEnum b
          + (#const STBTT_MACSTYLE_ITALIC) * fromEnum i
          + (#const STBTT_MACSTYLE_UNDERSCORE) * fromEnum u 

-- * Font

data Font = Font { getFont :: ForeignPtr Font }

sizeOfFont :: Int
sizeOfFont = #size stbtt_Font 

withFont :: Font -> (Ptr Font -> IO r) -> IO r
withFont = withForeignPtr . getFont

foreign import ccall "stb_truetype.h stbtt_GetNumberOfFonts" c_getNumberOfFonts :: Ptr CChar -> IO CInt
foreign import ccall "stb_truetype.h stbtt_GetFontOffsetforIndex" c_getFontOffsetForIndex :: Ptr CChar -> CInt -> IO CInt
foreign import ccall "stb_truetype.h stbtt_FindMatchingFont" c_findMatchingFont :: Ptr CChar -> Ptr CChar -> CInt -> IO CInt
foreign import ccall "stb_truetype.h stbtt_InitFont" c_initFont :: Ptr Font -> Ptr CChar -> CInt -> IO CInt
foreign import ccall "stb_truetype.h stbtt_FindGlyphIndex" c_findGlyphIndex :: Ptr Font -> CInt -> IO CInt
foreign import ccall "stb_truetype.h stbtt_ScaleForPixelHeight" c_scaleForPixelHeight :: Ptr Font -> Float -> IO Float
foreign import ccall "stb_truetype.h stbtt_ScaleForMappingEmToPixels" c_scaleForMappingEmToPixels :: Ptr Font -> Float -> IO Float
foreign import ccall "stb_truetype.h stbtt_GetFontVMetrics" c_getFontVMetrics :: Ptr Font -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "stb_truetype.h stbtt_GetFontVMetricsOS2" c_getFontVMetricsOS2 :: Ptr Font -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Cint
foreign import ccall "stb_truetype.h stbtt_GetFontBoundingBox" c_getFontBoundingBox :: Ptr Font -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "stb_truetype.h stbtt_IsGlyphEmpty" c_isGlyphEmpty :: Ptr Font -> CInt -> IO CInt

-- * newtype PackContext 

--stbtt_PackBegin
--stbtt_PackSetOversampling
--stbtt_PackFontRanges
--stbtt_PackEnd
--stbtt_GetPackedQuad

--stbtt_GetCodepointBitmap
--stbtt_MakeCodepointBitmap
--stbtt_GetCodepointBitmapBox

--stbtt_GetCodepointHMetrics
--stbtt_GetFontVMetrics
--stbtt_GetFontVMetricsOS2
--stbtt_GetCodepointKernAdvance()
