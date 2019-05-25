{-# language LambdaCase #-}

-- designed to be imported qualified
module Glow.Freetype.Atlas
  ( Atlas(..)
  , RenderDepth(..), c'renderDepth
  , new, delete
  , uploadIfNeeded, getTexture
  ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.TextureAtlas as TA
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.StateVar (($=))
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Error (throwIfNull)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Graphics.GL

import qualified Glow.Object as Glow
import Glow.Texture

data RenderDepth = LCD_FILTERING_ON | LCD_FILTERING_OFF

data Atlas =
    Atlas
    { ptr :: Ptr TA.C'texture_atlas_t
    , glTextureRef :: IORef (Maybe Texture) -- GL.TextureObject)
    , taUploadedRef :: IORef CSize
    }

getTexture :: Atlas -> IO Texture
getTexture atlas = readIORef (glTextureRef atlas) >>= \case
  Just texture -> return texture
  Nothing -> do
    texture <- Glow.gen 
    writeIORef (glTextureRef atlas) (Just texture)
    return texture

c'renderDepth :: RenderDepth -> CSize
c'renderDepth LCD_FILTERING_ON = TB.c'LCD_FILTERING_ON
c'renderDepth LCD_FILTERING_OFF = TB.c'LCD_FILTERING_OFF

delete :: Atlas -> IO ()
delete (Atlas p t _) = do
  TA.c'texture_atlas_delete p
  readIORef t >>= traverse_ Glow.delete

new
    :: Word -- ^ width
    -> Word -- ^ height
    -> RenderDepth
    -> IO Atlas
new width height depth
      = Atlas
    <$> throwIfNull "texture_atlas_new failed"
        ( TA.c'texture_atlas_new
          (fromIntegral width) (fromIntegral height) (c'renderDepth depth) )
    <*> newIORef Nothing
    <*> newIORef 0

upload :: Atlas -> IO ()
upload atlas = do
  texture <- getTexture atlas
  glBindTexture GL_TEXTURE_2D (Glow.object texture)
  texParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  texParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  texParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  texParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  -- ensure pixel storage format is correct
  glPixelStorei GL_UNPACK_LSB_FIRST 0
  glPixelStorei GL_UNPACK_SWAP_BYTES 0
  glPixelStorei GL_UNPACK_ROW_LENGTH 0
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_SKIP_ROWS 0
  glPixelStorei GL_UNPACK_SKIP_PIXELS 0
  glPixelStorei GL_UNPACK_SKIP_IMAGES 0
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  TA.C'texture_atlas_t width height depth _ _ dataPtr <- peek (ptr atlas)
  let internalFormat = if depth == 1 then GL_R8 else GL_RGB8
      pixelFormat = if depth == 1 then GL_RED else GL_RGB
  glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral width) (fromIntegral height) 0 pixelFormat GL_UNSIGNED_BYTE (castPtr dataPtr)

uploadIfNeeded :: Atlas -> IO ()
uploadIfNeeded atlas@(Atlas atlasPtr _ upRef) = do
  uploaded <- readIORef upRef
  used <- peek (TA.p'texture_atlas_t'used atlasPtr)
  when (uploaded /= used) $ do
    writeIORef upRef used
    upload atlas
