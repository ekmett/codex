{-# language CPP #-}
{-# language DeriveDataTypeable #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache 2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- So called RenderTargets or used for "Render to Texture". 
-- Memory to render into, usable as a texture or to read
-- otherwise from it. 
module Graphics.Glow.Framebuffer
( Framebuffer
-- * Binding
, boundFramebuffer
-- * Attaching
, FramebufferAttachment(attach)
, framebufferTexture
, framebufferRenderbuffer
, framebufferTextureLayer
-- * Completeness Check
, checkFramebufferStatus
-- * Framebuffer Targets
, FramebufferTarget
  ( FramebufferTarget
  , DrawFramebuffer
  , ReadFramebuffer
  , RWFramebuffer
  )
, FramebufferError
  ( FramebufferError
  , FramebufferComplete
  , FramebufferUndefined
  , FramebufferIncompleteAttachment
  , FramebufferIncompleteMissingAttachment
  , FramebufferIncompleteDrawBuffer
  , FramebufferIncompleteReadBuffer
  , FramebufferUnsupported
  , FramebufferIncompleteMultisample
  , FramebufferIncompleteLayerTargets
)
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Coerce
import Data.Data
import Data.Default
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.StateVar
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types

import Graphics.Glow.Object
import Graphics.Glow.Renderbuffer
import Graphics.Glow.Texture

newtype Framebuffer = Framebuffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

data FramebufferTarget = FramebufferTarget {-# unpack #-} !GLenum {-# unpack #-} !GLenum deriving (Eq,Ord,Typeable,Data,Generic)

pattern DrawFramebuffer :: FramebufferTarget
pattern DrawFramebuffer = FramebufferTarget GL_DRAW_FRAMEBUFFER GL_DRAW_FRAMEBUFFER_BINDING

pattern ReadFramebuffer :: FramebufferTarget
pattern ReadFramebuffer = FramebufferTarget GL_READ_FRAMEBUFFER GL_READ_FRAMEBUFFER_BINDING

pattern RWFramebuffer :: FramebufferTarget
pattern RWFramebuffer = FramebufferTarget GL_FRAMEBUFFER GL_FRAMEBUFFER_BINDING -- not sure if GL_FRAMEBUFFER_BINDING is still valid

instance Show FramebufferTarget where
  showsPrec d = \case
    DrawFramebuffer -> showString "DrawFramebuffer"
    ReadFramebuffer -> showString "ReadFramebuffer"
    RWFramebuffer -> showString "RWFramebuffer"
    FramebufferTarget x y -> showParen (d > 10) $ showString "FramebufferTarget " . showsPrec 11 x . showChar ' ' . showsPrec 11 y

newtype FramebufferError = FramebufferError GLenum deriving (Eq,Ord,Read,Typeable,Data,Generic)
type FramebufferAttachmentPoint = GLenum

instance Exception FramebufferError

pattern FramebufferComplete :: FramebufferError
pattern FramebufferComplete = FramebufferError GL_FRAMEBUFFER_COMPLETE

pattern FramebufferUndefined :: FramebufferError
pattern FramebufferUndefined = FramebufferError GL_FRAMEBUFFER_UNDEFINED

pattern FramebufferIncompleteAttachment :: FramebufferError
pattern FramebufferIncompleteAttachment = FramebufferError GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT

pattern FramebufferIncompleteMissingAttachment :: FramebufferError
pattern FramebufferIncompleteMissingAttachment = FramebufferError GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT

pattern FramebufferIncompleteDrawBuffer :: FramebufferError
pattern FramebufferIncompleteDrawBuffer = FramebufferError GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER

pattern FramebufferIncompleteReadBuffer :: FramebufferError
pattern FramebufferIncompleteReadBuffer = FramebufferError GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER

pattern FramebufferUnsupported :: FramebufferError
pattern FramebufferUnsupported = FramebufferError GL_FRAMEBUFFER_UNSUPPORTED

pattern FramebufferIncompleteMultisample :: FramebufferError
pattern FramebufferIncompleteMultisample = FramebufferError GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE

pattern FramebufferIncompleteLayerTargets :: FramebufferError
pattern FramebufferIncompleteLayerTargets = FramebufferError GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS

instance Show FramebufferError where
  showsPrec d = \ case
    FramebufferComplete -> showString "FramebufferComplete"
    FramebufferUndefined -> showString "FramebufferUndefined"
    FramebufferIncompleteAttachment -> showString "FramebufferIncompleteAttachment"
    FramebufferIncompleteMissingAttachment -> showString "FramebufferIncompleteMissingAttachment"
    FramebufferIncompleteDrawBuffer -> showString "FramebufferIncompleteDrawBuffer"
    FramebufferIncompleteReadBuffer -> showString "FramebufferIncompleteReadBuffer"
    FramebufferUnsupported -> showString "FramebufferUnsupported"
    FramebufferIncompleteMultisample -> showString "FramebufferIncompleteMultisample"
    FramebufferIncompleteLayerTargets -> showString "FramebufferIncompleteLayerTargets"
    FramebufferError t -> showParen (d > 10) $ showString "FramebufferError " . showsPrec 11 t

instance Object Framebuffer where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsFramebuffer (coerce i)
  deletes xs = liftIO $ allocaArray n \p -> do
    pokeArray p (coerce xs)
    glDeleteFramebuffers (fromIntegral n) p
    where n = length xs

instance Gen Framebuffer where
  gens n = liftIO $ allocaArray n \p -> do
    glGenFramebuffers (fromIntegral n) p
    map Framebuffer <$> peekArray n p

-- | The default Framebuffer is not just the null object but the screen buffer of the context
instance Default Framebuffer where
  def = Framebuffer 0

class FramebufferAttachment a where
  attach :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> a -> m ()

instance FramebufferAttachment Texture where
  attach target slot tex = framebufferTexture target slot tex 0

instance FramebufferAttachment (Renderbuffer a) where
  attach = framebufferRenderbuffer

-- * Binding

boundFramebuffer :: FramebufferTarget -> StateVar Framebuffer
boundFramebuffer (FramebufferTarget target binding) = StateVar g s where
  g = fmap (Framebuffer . fromIntegral) $ alloca $ liftM2 (>>) (glGetIntegerv binding) peek
  s = glBindFramebuffer target . coerce

-- * Attaching Buffer

-- | Attach a 'Texture' to the currently bound 'Framebuffer'
framebufferTexture :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Texture -> MipmapLevel -> m ()
framebufferTexture (FramebufferTarget t _) slot tex = liftIO . glFramebufferTexture t slot (object tex)

-- | Attach a single layer of a 'Texture' to the currently bound 'Framebuffer'
-- also usable to attach a cube map 'Texture' or cube map texture array
-- For cube map textures, layer is translated into a cube map face according to: face = k `mod` 6.
-- For cube map array textures, layer is translated into an array layer and face according to: layer = ceil (layer / 6) and face = k `mod` 6
framebufferTextureLayer :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Texture -> MipmapLevel -> TextureLayer -> m ()
framebufferTextureLayer (FramebufferTarget t _) slot tex level = liftIO . glFramebufferTextureLayer t slot (object tex) level

framebufferRenderbuffer :: MonadIO m => FramebufferTarget -> FramebufferAttachmentPoint -> Renderbuffer a -> m () 
framebufferRenderbuffer (FramebufferTarget t _) slot = liftIO . glFramebufferRenderbuffer t slot GL_RENDERBUFFER . object

checkFramebufferStatus :: MonadIO m => FramebufferTarget -> m (Maybe FramebufferError)
checkFramebufferStatus (FramebufferTarget t _) = do
  status <- glCheckFramebufferStatus t
  case status of
    GL_FRAMEBUFFER_COMPLETE -> return Nothing
    e -> return $ Just $ FramebufferError e

