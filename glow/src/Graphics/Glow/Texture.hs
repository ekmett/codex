{-# language CPP #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DataKinds #-}
{-# language OverloadedLists #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache 2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Glow.Texture
( Texture(..)
, TextureWrapping
, TextureMinificationFilter
, TextureMagnificationFilter
, MipmapLevel
, TextureLayer
, TextureUnit
-- * Texture Binding
, boundTexture
, withBoundTexture
-- * Texture Targets
, TextureTarget 
  ( TextureTarget
  , textureTarget
  , textureBinding
  , Texture1D
  , Texture1DArray
  , Texture2D
  , Texture2DArray
  , Texture2DMultisample
  , Texture2DMultisampleArray
  , Texture3D
  , TextureBuffer
  , TextureCubeMap
  , TextureRectangle
  )
-- * Texture Parameter
, TextureParameter
-- ** Bound Based
-- $texParameter
, texParameterf
, texParameteri
, texParameterfv
, texParameter2f
, texParameter3f
, texParameter4f
, texParameteriv
, texParameter2i
, texParameter3i
, texParameter4i
, texParameterIiv
, texParameterIuiv
-- * Texture Unit
, activeTexture
) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.StateVar
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Linear.V

import Graphics.Glow.Object

data TextureTarget = TextureTarget
  { textureTarget  :: {-# unpack #-} !GLenum
  , textureBinding :: {-# unpack #-} !GLenum
  } deriving (Eq,Ord,Show,Read)

pattern Texture1D = TextureTarget GL_TEXTURE_1D GL_TEXTURE_BINDING_1D
pattern Texture1DArray = TextureTarget GL_TEXTURE_1D_ARRAY GL_TEXTURE_BINDING_1D_ARRAY
pattern Texture2D = TextureTarget GL_TEXTURE_2D GL_TEXTURE_BINDING_2D
pattern Texture2DArray = TextureTarget GL_TEXTURE_2D_ARRAY GL_TEXTURE_BINDING_2D_ARRAY
pattern Texture2DMultisample = TextureTarget GL_TEXTURE_2D_MULTISAMPLE GL_TEXTURE_BINDING_2D_MULTISAMPLE
pattern Texture2DMultisampleArray = TextureTarget GL_TEXTURE_2D_MULTISAMPLE_ARRAY GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
pattern Texture3D = TextureTarget GL_TEXTURE_3D GL_TEXTURE_BINDING_3D
pattern TextureBuffer = TextureTarget GL_TEXTURE_BUFFER GL_TEXTURE_BINDING_BUFFER
pattern TextureCubeMap = TextureTarget GL_TEXTURE_CUBE_MAP GL_TEXTURE_BINDING_CUBE_MAP
pattern TextureRectangle = TextureTarget GL_TEXTURE_RECTANGLE GL_TEXTURE_BINDING_RECTANGLE

type TextureParameter = GLenum
type TextureWrapping = GLenum
type TextureMinificationFilter = GLenum
type TextureMagnificationFilter = GLenum
type MipmapLevel = GLsizei
type TextureLayer = GLint
type TextureUnit = GLuint

newtype Texture = Texture GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object Texture where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsTexture (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteTextures (fromIntegral n) p
    where n = length xs

instance Gen Texture where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenTextures (fromIntegral n) p
    map Texture <$> peekArray n p

instance Default Texture where
  def = Texture 0

boundTexture :: TextureTarget -> StateVar Texture
boundTexture (TextureTarget target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Texture (fromIntegral i)
  s = glBindTexture target . coerce

withBoundTexture :: TextureTarget -> Texture -> IO a -> IO a
withBoundTexture (boundTexture -> StateVar g s) v = bracket (g <* s v) s . const

-- * Texture Parameter

-- $texParameter
-- Settings for the current bound 'Texture'.
-- Consider using 'Graphics.Glow.Sampler' to store settings 'Texture' independent

texParameterf :: TextureTarget -> TextureParameter -> StateVar Float
texParameterf (TextureTarget t _) p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterfv t p . castPtr <*> peek
  s = glTexParameterf t p

texParameteri :: TextureTarget -> TextureParameter -> StateVar Int32
texParameteri (TextureTarget t _) p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameteriv t p . castPtr <*> peek
  s = glTexParameteri t p

texParameterfv' :: Storable (f Float) => TextureTarget -> TextureParameter -> StateVar (f Float)
texParameterfv' (TextureTarget t _) p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterfv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> glTexParameterfv t p . castPtr <*> (`poke` v)

texParameterfv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Float)
texParameterfv = texParameterfv'

texParameter2f :: TextureTarget -> TextureParameter -> StateVar (V2 Float)
texParameter2f = texParameterfv'

texParameter3f :: TextureTarget -> TextureParameter -> StateVar (V3 Float)
texParameter3f = texParameterfv'

texParameter4f :: TextureTarget -> TextureParameter -> StateVar (V4 Float)
texParameter4f = texParameterfv'

texParameteriv' :: Storable (f Int32) => TextureTarget -> TextureParameter -> StateVar (f Int32)
texParameteriv' (TextureTarget t _) p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameteriv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> glTexParameteriv t p . castPtr <*> (`poke` v)

texParameteriv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Int32)
texParameteriv = texParameteriv'

texParameter2i :: TextureTarget -> TextureParameter -> StateVar (V2 Int32)
texParameter2i = texParameteriv'

texParameter3i :: TextureTarget -> TextureParameter -> StateVar (V3 Int32)
texParameter3i = texParameteriv'

texParameter4i :: TextureTarget -> TextureParameter -> StateVar (V4 Int32)
texParameter4i = texParameteriv'

texParameterIiv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Int32)
texParameterIiv (TextureTarget t _) p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterIiv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> glTexParameterIiv t p . castPtr <*> (`poke` v)

texParameterIuiv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Word32)
texParameterIuiv (TextureTarget t _) p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterIuiv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> glTexParameterIuiv t p . castPtr <*> (`poke` v)

-- * Texture Unit
activeTexture :: StateVar Word32
activeTexture = StateVar g s where
  g = fmap fromIntegral $ alloca $ liftM2 (>>) (glGetIntegerv GL_ACTIVE_TEXTURE) peek
  s n = glActiveTexture (GL_TEXTURE0 + n)
