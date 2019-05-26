{-# language ForeignFunctionInterface #-}
module Graphics.FreeType.Internal where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.FreeType.Types

foreign import ccall "FT_Activate_Size" activateSize :: Size -> IO Error
foreign import ccall "FT_Attach_File" attachFile :: Ptr Face -> CString -> IO Error
foreign import ccall "FT_Attach_Stream" attachStream :: Ptr Face -> Ptr OpenArgs -> IO Error
foreign import ccall "FT_Done_Face" doneFace :: Ptr Face -> IO Error
foreign import ccall "FT_Done_FreeType" doneFreeType :: Ptr Library -> IO Error
foreign import ccall "FT_Done_Glyph" doneGlyph :: Glyph -> IO ()
foreign import ccall "FT_Done_Size" doneSize :: Size -> IO Error
foreign import ccall "FT_Face_GetCharVariantIndex" faceGetCharVariantIndex :: Ptr Face -> CULong -> CULong -> IO CUInt
foreign import ccall "FT_Face_GetCharVariantIsDefault" faceGetCharVariantIsDefault :: Ptr Face -> CULong -> CULong -> IO CInt
foreign import ccall "FT_Face_GetCharsOfVariant" faceGetCharsOfVariant :: Ptr Face -> CULong -> IO (Ptr Word32)
foreign import ccall "FT_Face_GetVariantSelectors" faceGetVariantSelectors :: Ptr Face -> IO (Ptr Word32)
foreign import ccall "FT_Face_GetVariantsOfChar" faceGetVariantsOfChar :: Ptr Face -> CULong -> IO (Ptr Word32)
foreign import ccall "FT_Get_Char_Index" getCharIndex :: Ptr Face -> CULong -> IO CUInt
foreign import ccall "FT_Get_Charmap_Index" getCharmapIndex :: CharMap -> IO CInt
foreign import ccall "FT_Get_FSType_Flags" getFSTypeFlags :: Ptr Face -> IO CUShort
foreign import ccall "FT_Get_First_Char" getFirstChar :: Ptr Face -> Ptr CUInt -> IO CULong 
foreign import ccall "FT_Get_Glyph" getGlyph :: GlyphSlot -> Ptr Glyph -> IO Error
foreign import ccall "FT_Get_Glyph_Name" getGlyphName :: Ptr Face -> CUInt -> Ptr () -> CUInt -> IO Error
foreign import ccall "FT_Get_Kerning" getKerning :: Ptr Face -> CUInt -> CUInt -> CUInt -> Ptr Vector -> IO Error
foreign import ccall "FT_Get_Name_Index" getNameIndex :: Ptr Face -> CString -> IO CUInt
foreign import ccall "FT_Get_Next_Char" getNextChar :: Ptr Face -> CULong -> Ptr CUInt -> IO CULong
foreign import ccall "FT_Get_Postscript_Name" getPostscriptName :: Ptr Face -> IO CString
foreign import ccall "FT_Get_SubGlyph_Info" getSubGlyphInfo :: GlyphSlot -> CUInt -> Ptr CInt -> Ptr CUInt -> Ptr CInt -> Ptr CInt -> Ptr Matrix -> IO Error
foreign import ccall "FT_Get_Track_Kerning" getTrackKerning :: Ptr Face -> Fixed -> CInt -> Ptr Fixed -> IO Error
foreign import ccall "FT_Glyph_To_Bitmap" glyphToBitmap :: Ptr Glyph  -> RenderMode -> Ptr Vector -> FT_Bool -> IO Error
foreign import ccall "FT_Init_FreeType" initFreeType :: Ptr (Ptr Library) -> IO Error
foreign import ccall "FT_Library_Version" libraryVersion :: Ptr Library -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "FT_Load_Char" loadChar :: Ptr Face -> CULong -> Int32 -> IO Error
foreign import ccall "FT_Load_Glyph" loadGlyph :: Ptr Face -> CUInt -> Int32 -> IO Error
foreign import ccall "FT_New_Face" newFace :: Ptr Library -> CString -> CLong -> Ptr (Ptr Face) -> IO Error
foreign import ccall "FT_New_Memory_Face" newMemoryFace :: Ptr Library -> Bytes -> CLong -> CLong -> Ptr Face -> IO Error
foreign import ccall "FT_New_Size" newSize :: Ptr Face -> Ptr Size -> IO Error
foreign import ccall "FT_Open_Face" openFace :: Ptr Library -> Ptr OpenArgs -> CLong -> Ptr (Ptr Face) -> IO Error
foreign import ccall "FT_Outline_Check" outlineCheck :: Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Copy" outlineCopy :: Ptr Outline -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Decompose" outlineDecompose :: Ptr Outline -> Ptr OutlineFuncs -> Ptr a -> IO Error
foreign import ccall "FT_Outline_Done" outlineDone :: Ptr Library -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Done_Internal" outlineDoneInternal :: Memory -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Embolden" outlineEmbolden :: Ptr Outline -> Pos -> IO Error
foreign import ccall "FT_Outline_Get_BBox" outlineGetBBox :: Ptr Outline -> Ptr BBox -> IO Error
foreign import ccall "FT_Outline_Get_Bitmap" outlineGetBitmap :: Ptr Library -> Ptr Outline -> Ptr Bitmap -> IO Error
foreign import ccall "FT_Outline_Get_CBox" outlineGetCBox :: Ptr Outline -> Ptr BBox -> IO ()
foreign import ccall "FT_Outline_Get_Orientation" outlineGetOrientation :: Ptr Outline -> IO Orientation
foreign import ccall "FT_Outline_New" outlineNew :: Ptr Library -> CUInt -> CInt -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_New_Internal" outlineNewInternal :: Memory -> CUInt -> CInt -> Ptr Outline -> IO Error
foreign import ccall "FT_Outline_Render" outlineRender :: Ptr Library -> Ptr Outline -> Ptr RasterParams -> IO Error
foreign import ccall "FT_Outline_Reverse" outlineReverse :: Ptr Outline -> IO ()
foreign import ccall "FT_Outline_Transform" outlineTransform :: Ptr Outline -> Ptr Matrix -> IO ()
foreign import ccall "FT_Outline_Translate" outlineTranslate :: Ptr Outline -> Pos -> Pos -> IO ()
foreign import ccall "FT_Reference_Face" referenceFace :: Ptr Face -> IO Error
foreign import ccall "FT_Render_Glyph" renderGlyph :: GlyphSlot -> RenderMode -> IO Error
foreign import ccall "FT_Request_Size" requestSize :: Ptr Face -> SizeRequest -> IO Error
foreign import ccall "FT_Select_Charmap" selectCharmap :: Ptr Face -> Encoding -> IO Error
foreign import ccall "FT_Select_Size" selectSize :: Ptr Face -> CInt -> IO Error
foreign import ccall "FT_Set_Char_Size" setCharSize :: Ptr Face -> F26Dot6 -> F26Dot6 -> CUInt -> CUInt -> IO Error
foreign import ccall "FT_Set_Charmap" setCharmap :: Ptr Face -> CharMap -> IO Error
foreign import ccall "FT_Set_Pixel_Sizes" setPixelSizes :: Ptr Face -> CUInt -> CUInt -> IO Error
foreign import ccall "FT_Set_Transform" setTransform :: Ptr Face -> Ptr Matrix -> Ptr Vector -> IO ()