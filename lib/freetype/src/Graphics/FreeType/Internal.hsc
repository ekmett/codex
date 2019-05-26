module Graphics.FreeType.Internal where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.FreeType.Face
import Graphics.FreeType.Matrix
import Graphics.FreeType.Vector
import Graphics.FreeType.Glyph
import Graphics.FreeType.GlyphSlot
import Graphics.FreeType.OpenArgs
import Graphics.FreeType.SizeRequest
import Graphics.FreeType.CharMap
import Graphics.FreeType.Outline
import Graphics.FreeType.Memory
import Graphics.FreeType.BBox
import Graphics.FreeType.Bitmap
import Graphics.FreeType.RasterParams
import Graphics.FreeType.Size
import Graphics.FreeType.Types

#include <ft2build.h>
#include FT_FREETYPE_H

foreign import ccall "FT_Activate_Size" activateSize :: Size -> IO Error
foreign import ccall "FT_Attach_File" attachFile :: Face -> CString -> IO Error
foreign import ccall "FT_Attach_Stream" attachStream :: Face -> Ptr OpenArgs -> IO Error
foreign import ccall "FT_Done_Face" doneFace :: Face -> IO Error
foreign import ccall "FT_Done_FreeType" doneFreeType :: Library -> IO Error
foreign import ccall "FT_Done_Glyph" doneGlyph :: Glyph -> IO ()
foreign import ccall "FT_Done_Size" doneSize :: Size -> IO Error
-- | This is just here for completeness,
-- TrueType hinting is no longer patented
foreign import ccall "FT_Face_CheckTrueTypePatents" faceCheckTrueTypePatents :: Face -> IO FT_Bool
foreign import ccall "FT_Face_GetCharVariantIndex" faceGetCharVariantIndex :: Face -> CULong -> CULong -> IO CUInt
foreign import ccall "FT_Face_GetCharVariantIsDefault" faceGetCharVariantIsDefault :: Face -> CULong -> CULong -> IO CInt
foreign import ccall "FT_Face_GetCharsOfVariant" faceGetCharsOfVariant :: Face -> CULong -> IO (Ptr Word32)
foreign import ccall "FT_Face_GetVariantSelectors" faceGetVariantSelectors :: Face -> IO (Ptr Word32)
foreign import ccall "FT_Face_GetVariantsOfChar" faceGetVariantsOfChar :: Face -> CULong -> IO (Ptr Word32)
-- | This is just here for completeness,
-- TrueType hinting is no longer patented
foreign import ccall "FT_Face_SetUnpatentedHinting" faceSetUnpatentedHinting :: Face -> FT_Bool -> IO FT_Bool
foreign import ccall "FT_Get_Char_Index" getCharIndex :: Face -> CULong -> IO CUInt
foreign import ccall "FT_Get_Charmap_Index" getCharmapIndex :: CharMap -> IO CInt
foreign import ccall "FT_Get_FSType_Flags" getFSTypeFlags :: Face -> IO CUShort
foreign import ccall "FT_Get_First_Char" getFirstChar :: Face -> Ptr CUInt -> IO CULong 
foreign import ccall "FT_Get_Glyph" getGlyph :: GlyphSlot -> Ptr Glyph -> IO Error
foreign import ccall "FT_Get_Glyph_Name" getGlyphName :: Face -> CUInt -> Ptr () -> CUInt -> IO Error
foreign import ccall "FT_Get_Kerning" getKerning :: Face -> CUInt -> CUInt -> CUInt -> Ptr Vector -> IO Error
foreign import ccall "FT_Get_Name_Index" getNameIndex :: Face -> CString -> IO CUInt
foreign import ccall "FT_Get_Next_Char" getNextChar :: Face -> CULong -> Ptr CUInt -> IO CULong
foreign import ccall "FT_Get_Postscript_Name" getPostscriptName :: Face -> IO CString
foreign import ccall "FT_Get_SubGlyph_Info" getSubGlyphInfo :: GlyphSlot -> CUInt -> Ptr CInt -> Ptr CUInt -> Ptr CInt -> Ptr CInt -> Ptr Matrix -> IO Error
foreign import ccall "FT_Get_Track_Kerning" getTrackKerning :: Face -> Fixed -> CInt -> Ptr Fixed -> IO Error
foreign import ccall "FT_Glyph_To_Bitmap" glyphToBitmap :: Ptr Glyph  -> RenderMode -> Ptr Vector -> FT_Bool -> IO Error
foreign import ccall "FT_Init_FreeType" initFreeType :: Ptr Library -> IO Error
foreign import ccall "FT_Library_Version" libraryVersion :: Library -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "FT_Load_Char" loadChar :: Face -> CULong -> Int32 -> IO Error
foreign import ccall "FT_Load_Glyph" loadGlyph :: Face -> CUInt -> Int32 -> IO Error
foreign import ccall "FT_New_Face" newFace :: Library  -> CString -> CLong -> Ptr Face -> IO Error
foreign import ccall "FT_New_Memory_Face" newMemoryFace :: Library -> Bytes -> CLong -> CLong -> Ptr Face -> IO Error
foreign import ccall "FT_New_Size" newSize :: Face -> Ptr Size -> IO Error
foreign import ccall "FT_Open_Face" openFace :: Library -> Ptr OpenArgs -> CLong -> Ptr Face -> IO Error
foreign import ccall "FT_Outline_Check" outlineCheck :: Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Copy" outlineCopy :: Ptr Outline -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Decompose" outlineDecompose :: Ptr Outline -> Ptr OutlineFuncs -> Ptr a -> IO Error
foreign import ccall "FT_Outline_Done" outlineDone :: Library -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Done_Internal" outlineDoneInternal :: Memory -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Embolden" outlineEmbolden :: Ptr Outline -> Pos -> IO Error
foreign import ccall "FT_Outline_Get_BBox" outlineGetBBox :: Ptr Outline -> Ptr BBox -> IO Error
foreign import ccall "FT_Outline_Get_Bitmap" outlineGetBitmap :: Library -> Ptr Outline -> Ptr Bitmap -> IO Error
foreign import ccall "FT_Outline_Get_CBox" outlineGetCBox :: Ptr Outline -> Ptr BBox -> IO ()
foreign import ccall "FT_Outline_Get_Orientation" outlineGetOrientation :: Ptr Outline -> IO Orientation
foreign import ccall "FT_Outline_New" outlineNew :: Library -> CUInt -> CInt -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_New_Internal" outlineNewInternal :: Memory -> CUInt -> CInt -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Render" outlineRender :: Library -> Ptr Outline -> Ptr RasterParams -> IO Error
foreign import ccall "FT_Outline_Reverse" outlineReverse :: Ptr Outline -> IO ()
foreign import ccall "FT_Outline_Transform" outlineTransform :: Ptr Outline -> Ptr Matrix -> IO ()
foreign import ccall "FT_Outline_Translate" outlineTranslate :: Ptr Outline -> Pos -> Pos -> IO ()
foreign import ccall "FT_Reference_Face" referenceFace :: Face -> IO Error
foreign import ccall "FT_Render_Glyph" renderGlyph :: GlyphSlot -> RenderMode -> IO Error
foreign import ccall "FT_Request_Size" requestSize :: Face -> SizeRequest -> IO Error
foreign import ccall "FT_Select_Charmap" selectCharmap :: Face -> Encoding -> IO Error
foreign import ccall "FT_Select_Size" selectSize :: Face -> CInt -> IO Error
foreign import ccall "FT_Set_Char_Size" setCharSize :: Face -> F26Dot6 -> F26Dot6 -> CUInt -> CUInt -> IO Error
foreign import ccall "FT_Set_Charmap" setCharmap :: Face -> CharMap -> IO Error
foreign import ccall "FT_Set_Pixel_Sizes" setPixelSizes :: Face -> CUInt -> CUInt -> IO Error
foreign import ccall "FT_Set_Transform" setTransform :: Face -> Ptr Matrix -> Ptr Vector -> IO ()
