{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.FreeType.Internal where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.FreeType.Internal.PrimitiveTypes
import Graphics.FreeType.Internal.Library
import Graphics.FreeType.Internal.Face
import Graphics.FreeType.Internal.Matrix
import Graphics.FreeType.Internal.Vector
import Graphics.FreeType.Internal.Glyph
import Graphics.FreeType.Internal.GlyphSlot
import Graphics.FreeType.Internal.OpenArgs
import Graphics.FreeType.Internal.SizeRequest
import Graphics.FreeType.Internal.CharMap
import Graphics.FreeType.Internal.Outline
import Graphics.FreeType.Internal.Memory
import Graphics.FreeType.Internal.BBox
import Graphics.FreeType.Internal.Bitmap
import Graphics.FreeType.Internal.RasterParams
import Graphics.FreeType.Internal.Size

#include <ft2build.h>
#include FT_FREETYPE_H

foreign import ccall "FT_Activate_Size" _FT_Activate_Size :: FT_Size -> IO FT_Error
foreign import ccall "FT_Attach_File" _FT_Attach_File :: FT_Face -> CString -> IO FT_Error
foreign import ccall "FT_Attach_Stream" _FT_Attach_Stream :: FT_Face -> Ptr FT_Open_Args -> IO FT_Error
foreign import ccall "FT_Done_Face" _FT_Done_Face :: FT_Face -> IO FT_Error
foreign import ccall "FT_Done_FreeType" _FT_Done_FreeType :: FT_Library -> IO FT_Error
foreign import ccall "FT_Done_Glyph" _FT_Done_Glyph :: FT_Glyph -> IO ()
foreign import ccall "FT_Done_Size" _FT_Done_Size :: FT_Size -> IO FT_Error
-- | This is just here for completeness,
-- TrueType hinting is no longer patented
foreign import ccall "FT_Face_CheckTrueTypePatents" _FT_Face_CheckTrueTypePatents :: FT_Face -> IO FT_Bool
foreign import ccall "FT_Face_GetCharVariantIndex" _FT_Face_GetCharVariantIndex :: FT_Face -> FT_ULong -> FT_ULong -> IO FT_UInt
foreign import ccall "FT_Face_GetCharVariantIsDefault" _FT_Face_GetCharVariantIsDefault :: FT_Face -> FT_ULong -> FT_ULong -> IO FT_Int
foreign import ccall "FT_Face_GetCharsOfVariant" _FT_Face_GetCharsOfVariant :: FT_Face -> FT_ULong -> IO (Ptr FT_UInt32)
foreign import ccall "FT_Face_GetVariantSelectors" _FT_Face_GetVariantSelectors :: FT_Face -> IO (Ptr FT_UInt32)
foreign import ccall "FT_Face_GetVariantsOfChar" _FT_Face_GetVariantsOfChar :: FT_Face -> FT_ULong -> IO (Ptr FT_UInt32)
-- | This is just here for completeness,
-- TrueType hinting is no longer patented
foreign import ccall "FT_Face_SetUnpatentedHinting" _FT_Face_SetUnpatentedHinting :: FT_Face -> FT_Bool -> IO FT_Bool
foreign import ccall "FT_Get_Char_Index" _FT_Get_Char_Index :: FT_Face -> FT_ULong -> IO FT_UInt
foreign import ccall "FT_Get_Charmap_Index" _FT_Get_Charmap_Index :: FT_CharMap -> IO FT_Int
foreign import ccall "FT_Get_FSType_Flags" _FT_Get_FSType_Flags :: FT_Face -> IO FT_UShort
foreign import ccall "FT_Get_First_Char" _FT_Get_First_Char :: FT_Face -> Ptr FT_UInt -> IO FT_ULong 
foreign import ccall "FT_Get_Glyph" _FT_Get_Glyph :: FT_GlyphSlot -> Ptr FT_Glyph -> IO FT_Error
foreign import ccall "FT_Get_Glyph_Name" _FT_Get_Glyph_Name :: FT_Face -> FT_UInt -> FT_Pointer -> FT_UInt -> IO FT_Error
foreign import ccall "FT_Get_Kerning" _FT_Get_Kerning :: FT_Face -> FT_UInt -> FT_UInt -> FT_UInt -> Ptr FT_Vector -> IO FT_Error
foreign import ccall "FT_Get_Name_Index" _FT_Get_Name_Index :: FT_Face -> CString -> IO FT_UInt
foreign import ccall "FT_Get_Next_Char" _FT_Get_Next_Char :: FT_Face -> FT_ULong -> Ptr FT_UInt -> IO FT_ULong
foreign import ccall "FT_Get_Postscript_Name" _FT_Get_Postscript_Name :: FT_Face -> IO CString
foreign import ccall "FT_Get_SubGlyph_Info" _FT_Get_SubGlyph_Info :: FT_GlyphSlot -> FT_UInt -> Ptr FT_Int -> Ptr FT_UInt -> Ptr FT_Int -> Ptr FT_Int -> Ptr FT_Matrix -> IO FT_Error
foreign import ccall "FT_Get_Track_Kerning" _FT_Get_Track_Kerning :: FT_Face -> FT_Fixed -> FT_Int -> Ptr FT_Fixed -> IO FT_Error
foreign import ccall "FT_Glyph_To_Bitmap" _FT_Glyph_To_Bitmap :: Ptr FT_Glyph  -> FT_Render_Mode -> Ptr FT_Vector -> FT_Bool -> IO FT_Error
foreign import ccall "FT_Init_FreeType" _FT_Init_FreeType :: Ptr FT_Library -> IO FT_Error
foreign import ccall "FT_Library_Version" _FT_Library_Version :: FT_Library -> Ptr FT_Int -> Ptr FT_Int -> Ptr FT_Int -> IO ()
foreign import ccall "FT_Load_Char" _FT_Load_Char :: FT_Face -> FT_ULong -> FT_Int32 -> IO FT_Error
foreign import ccall "FT_Load_Glyph" _FT_Load_Glyph :: FT_Face -> FT_UInt -> FT_Int32 -> IO FT_Error
foreign import ccall "FT_New_Face" _FT_New_Face :: FT_Library  -> CString -> FT_Long -> Ptr FT_Face -> IO FT_Error
foreign import ccall "FT_New_Memory_Face" _FT_New_Memory_Face :: FT_Library -> FT_Bytes -> FT_Long -> FT_Long -> Ptr FT_Face -> IO FT_Error
foreign import ccall "FT_New_Size" _FT_New_Size :: FT_Face -> Ptr FT_Size -> IO FT_Error
foreign import ccall "FT_Open_Face" _FT_Open_Face :: FT_Library -> Ptr FT_Open_Args -> FT_Long -> Ptr FT_Face -> IO FT_Error
foreign import ccall "FT_Outline_Check" _FT_Outline_Check :: Ptr FT_Outline -> IO FT_Error
foreign import ccall "FT_Outline_Copy" _FT_Outline_Copy :: Ptr FT_Outline -> Ptr FT_Outline -> IO FT_Error
foreign import ccall "FT_Outline_Decompose" _FT_Outline_Decompose :: Ptr FT_Outline -> Ptr FT_Outline_Funcs -> Ptr a -> IO FT_Error
foreign import ccall "FT_Outline_Done" _FT_Outline_Done :: FT_Library -> Ptr FT_Outline -> IO FT_Error
foreign import ccall "FT_Outline_Done_Internal" _FT_Outline_Done_Internal :: FT_Memory -> Ptr FT_Outline -> IO FT_Error
foreign import ccall "FT_Outline_Embolden" _FT_Outline_Embolden :: Ptr FT_Outline -> FT_Pos -> IO FT_Error
foreign import ccall "FT_Outline_Get_BBox" _FT_Outline_Get_BBox :: Ptr FT_Outline -> Ptr FT_BBox -> IO FT_Error
foreign import ccall "FT_Outline_Get_Bitmap" _FT_Outline_Get_Bitmap :: FT_Library -> Ptr FT_Outline -> Ptr FT_Bitmap -> IO FT_Error
foreign import ccall "FT_Outline_Get_CBox" _FT_Outline_Get_CBox :: Ptr FT_Outline -> Ptr FT_BBox -> IO ()
foreign import ccall "FT_Outline_Get_Orientation" _FT_Outline_Get_Orientation :: Ptr FT_Outline -> IO FT_Orientation
foreign import ccall "FT_Outline_New" _FT_Outline_New :: FT_Library -> FT_UInt -> FT_Int -> Ptr FT_Outline -> IO FT_Error
foreign import ccall "FT_Outline_New_Internal" _FT_Outline_New_Internal :: FT_Memory -> FT_UInt -> FT_Int -> Ptr FT_Outline -> IO FT_Error
foreign import ccall "FT_Outline_Render" _FT_Outline_Render :: FT_Library -> Ptr FT_Outline -> Ptr FT_Raster_Params -> IO FT_Error
foreign import ccall "FT_Outline_Reverse" _FT_Outline_Reverse :: Ptr FT_Outline -> IO ()
foreign import ccall "FT_Outline_Transform" _FT_Outline_Transform :: Ptr FT_Outline -> Ptr FT_Matrix -> IO ()
foreign import ccall "FT_Outline_Translate" _FT_Outline_Translate :: Ptr FT_Outline -> FT_Pos -> FT_Pos -> IO ()
foreign import ccall "FT_Reference_Face" _FT_Reference_Face :: FT_Face -> IO FT_Error
foreign import ccall "FT_Render_Glyph" _FT_Render_Glyph :: FT_GlyphSlot -> FT_Render_Mode -> IO FT_Error
foreign import ccall "FT_Request_Size" _FT_Request_Size :: FT_Face -> FT_Size_Request -> IO FT_Error
foreign import ccall "FT_Select_Charmap" _FT_Select_Charmap :: FT_Face -> FT_Encoding -> IO FT_Error
foreign import ccall "FT_Select_Size" _FT_Select_Size :: FT_Face -> FT_Int -> IO FT_Error
foreign import ccall "FT_Set_Char_Size" _FT_Set_Char_Size :: FT_Face -> FT_F26Dot6 -> FT_F26Dot6 -> FT_UInt -> FT_UInt    -> IO FT_Error
foreign import ccall "FT_Set_Charmap" _FT_Set_Charmap :: FT_Face -> FT_CharMap -> IO FT_Error
foreign import ccall "FT_Set_Pixel_Sizes" _FT_Set_Pixel_Sizes :: FT_Face -> FT_UInt -> FT_UInt -> IO FT_Error
foreign import ccall "FT_Set_Transform" _FT_Set_Transform :: FT_Face -> Ptr FT_Matrix -> Ptr FT_Vector -> IO ()
