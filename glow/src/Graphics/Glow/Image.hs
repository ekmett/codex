{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache 2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Transcoding between @JuicyPixels@ and @gl@
module Graphics.Glow.Image
( 
-- * Uploading to OpenGL
  Image2D(upload,store)
-- * Downloading from OpenGL
, download, downloadM
-- * Image formats
, ImageFormat(..)
) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Proxy
import Data.Vector.Storable as V
import Data.Vector.Storable.Mutable as MV
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.GL.Core41
import Graphics.GL.Ext.ARB.TextureStorage

import Graphics.Glow.Pixel
import Graphics.Glow.Texture

-- * 2D Image formats

class Storable (PixelBaseComponent a) => ImageFormat a where
  internalFormat  :: p a -> InternalFormat
  pixelFormat     :: p a -> PixelFormat
  pixelType       :: p a -> PixelType
  swizzle         :: p a -> TextureTarget -> IO ()
  swizzle _ _ = return ()

instance ImageFormat PixelRGB8 where
  internalFormat  _ = GL_RGB8
  pixelFormat     _ = GL_RGB
  pixelType       _ = GL_UNSIGNED_BYTE

instance ImageFormat PixelRGB16 where
  internalFormat  _ = GL_RGB16
  pixelFormat     _ = GL_RGB
  pixelType       _ = GL_UNSIGNED_SHORT

instance ImageFormat PixelRGBA8 where
  internalFormat  _ = GL_RGBA8
  pixelFormat     _ = GL_RGBA
  pixelType       _ = GL_UNSIGNED_BYTE

instance ImageFormat PixelRGBA16 where
  internalFormat  _ = GL_RGBA16
  pixelFormat     _ = GL_RGBA
  pixelType       _ = GL_UNSIGNED_SHORT

instance ImageFormat PixelRGBF where
  internalFormat _ = GL_RGB32F
  pixelFormat    _ = GL_RGB
  pixelType      _ = GL_FLOAT

swizzleL :: TextureTarget -> IO ()
swizzleL t = allocaArray 4 $ \p -> do
  pokeArray p [GL_RED, GL_RED, GL_RED, GL_ONE]
  glTexParameteriv t GL_TEXTURE_SWIZZLE_RGBA p

swizzleLA :: TextureTarget -> IO ()
swizzleLA t = allocaArray 4 $ \p -> do
  pokeArray p [GL_RED, GL_RED, GL_RED, GL_GREEN]
  glTexParameteriv t GL_TEXTURE_SWIZZLE_RGBA p

instance ImageFormat Word8 where
  internalFormat _ = GL_R8
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_UNSIGNED_BYTE
  swizzle        _ = swizzleL

instance ImageFormat Word16 where
  internalFormat _ = GL_R16
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_UNSIGNED_SHORT
  swizzle        _ = swizzleL

instance ImageFormat Word32 where
  internalFormat _ = GL_R32UI
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_UNSIGNED_INT
  swizzle        _ = swizzleL

instance ImageFormat Float where
  internalFormat _ = GL_R32F
  pixelFormat    _ = GL_RED
  pixelType      _ = GL_FLOAT
  swizzle        _ = swizzleL

instance ImageFormat PixelYA8 where
  internalFormat _ = GL_RG8
  pixelFormat    _ = GL_RG
  pixelType      _ = GL_UNSIGNED_BYTE
  swizzle        _ = swizzleLA

instance ImageFormat PixelYA16 where
  internalFormat _ = GL_RG16
  pixelFormat    _ = GL_RG
  pixelType      _ = GL_UNSIGNED_SHORT
  swizzle        _ = swizzleLA

-- * Download

packedPixelStore :: IO ()
packedPixelStore = do
  glPixelStorei GL_PACK_LSB_FIRST    0
  glPixelStorei GL_PACK_SWAP_BYTES   0
  glPixelStorei GL_PACK_ROW_LENGTH   0
  glPixelStorei GL_PACK_IMAGE_HEIGHT 0
  glPixelStorei GL_PACK_SKIP_ROWS    0
  glPixelStorei GL_PACK_SKIP_PIXELS  0
  glPixelStorei GL_PACK_SKIP_IMAGES  0
  glPixelStorei GL_PACK_ALIGNMENT    1 -- normally 4!


-- @'download' x y w h@ copies w*h region of the screen starting from position (x,y) into a JuicyPixels image
download :: forall m a. (MonadIO m, ImageFormat a) => Int -> Int -> Int -> Int -> m (Image a)
download x y w h
  | w >= 0, h >= 0, n <- w * h = liftIO $ do
    fp <- mallocForeignPtrArray (compSize2D 1 fmt typ w h)
    packedPixelStore
    withForeignPtr fp $ glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) fmt typ . castPtr
    return $ Image w h $ V.unsafeFromForeignPtr fp 0 n
  | otherwise = error "download: bad size"
  where fmt = pixelFormat (Proxy :: Proxy a)
        typ = pixelType   (Proxy :: Proxy a)

-- | @'downloadM' x0 y0@ copies the screen starting at position (x,y) into an existing mutable image
downloadM :: forall m a. (MonadIO m, ImageFormat a) => Int -> Int -> MutableImage RealWorld a -> m ()
downloadM x y (MutableImage w h mv) = liftIO $ do
    packedPixelStore
    MV.unsafeWith mv $ glReadPixels
      (fromIntegral x) (fromIntegral y)
      (fromIntegral w) (fromIntegral h)
      (pixelFormat (Proxy :: Proxy a))
      (pixelType   (Proxy :: Proxy a)) . castPtr

-- * Upload

class Image2D i where
  upload :: MonadIO m => i -> TextureTarget -> MipmapLevel -> m ()
  store :: MonadIO m => i -> TextureTarget -> m ()

instance ImageFormat a => Image2D (Image a) where
  upload i@(Image w h v) t l= liftIO $ do
    packedPixelStore
    V.unsafeWith v $ glTexSubImage2D t l 0 0 (fromIntegral w) (fromIntegral h) (pixelFormat i) (pixelType i) . castPtr
    swizzle i t
  store i@(Image w h _) t = liftIO $ do
    glTexStorage2D t 1 (internalFormat i) (fromIntegral w) (fromIntegral h)
    upload i t 0

instance (ImageFormat a, s ~ RealWorld) => Image2D (MutableImage s a) where
  upload i@(MutableImage w h v) t l = liftIO $ do
    packedPixelStore
    MV.unsafeWith v $ glTexSubImage2D t l 0 0 (fromIntegral w) (fromIntegral h) (pixelFormat i) (pixelType i) . castPtr
    swizzle i t
  store i@(MutableImage w h _) t = liftIO $ do
    glTexStorage2D t 1 (internalFormat i) (fromIntegral w) (fromIntegral h)
    upload i t 0

instance Image2D DynamicImage where
  upload (ImageY8 i)     = upload i
  upload (ImageY16 i)    = upload i
  upload (ImageY32 i)    = upload i
  upload (ImageYF i)     = upload i
  upload (ImageYA8 i)    = upload i
  upload (ImageYA16 i)   = upload i
  upload (ImageRGB8 i)   = upload i
  upload (ImageRGB16 i)  = upload i
  upload (ImageRGBF i)   = upload i
  upload (ImageRGBA8 i)  = upload i
  upload (ImageRGBA16 i) = upload i
  upload (ImageYCbCr8 i) = upload (convertImage i :: Image PixelRGB8)
  upload (ImageCMYK8 i)  = upload (convertImage i :: Image PixelRGB8)
  upload (ImageCMYK16 i) = upload (convertImage i :: Image PixelRGB16)

  store (ImageY8 i)     = store i
  store (ImageY16 i)    = store i
  store (ImageY32 i)    = store i
  store (ImageYF i)     = store i
  store (ImageYA8 i)    = store i
  store (ImageYA16 i)   = store i
  store (ImageRGB8 i)   = store i
  store (ImageRGB16 i)  = store i
  store (ImageRGBF i)   = store i
  store (ImageRGBA8 i)  = store i
  store (ImageRGBA16 i) = store i
  store (ImageYCbCr8 i) = store (convertImage i :: Image PixelRGB8)
  store (ImageCMYK8 i)  = store (convertImage i :: Image PixelRGB8)
  store (ImageCMYK16 i) = store (convertImage i :: Image PixelRGB16)
