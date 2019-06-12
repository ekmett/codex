{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language BlockArguments #-}
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
--
module Graphics.Glow.Renderbuffer
( Renderbuffer
, RenderbufferTarget
  ( RenderbufferTargeting
  , RenderbufferTarget
  )
, boundRenderbuffer
) where

import Control.Monad
import Control.Monad.IO.Class
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

newtype Renderbuffer a = Renderbuffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object (Renderbuffer a) where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsRenderbuffer (coerce i)
  deletes xs = liftIO $ allocaArray n \p -> do
    pokeArray p (coerce xs)
    glDeleteRenderbuffers (fromIntegral n) p
    where n = length xs

instance Gen (Renderbuffer a) where
  gens n = liftIO $ allocaArray n \p -> do
    glGenRenderbuffers (fromIntegral n) p
    map Renderbuffer <$> peekArray n p

instance Default (Renderbuffer a) where
  def = Renderbuffer 0

data RenderbufferTarget = RenderbufferTargeting GLenum GLenum deriving (Eq,Ord,Typeable,Data,Generic)

pattern RenderbufferTarget :: RenderbufferTarget
pattern RenderbufferTarget = RenderbufferTargeting GL_RENDERBUFFER GL_RENDERBUFFER_BINDING

instance Show RenderbufferTarget where
  showsPrec d = \case
    RenderbufferTarget -> showString "RenderbufferTarget"
    RenderbufferTargeting x y -> showParen (d > 10) $ showString "RenderbufferTargeting " . showsPrec 11 x . showChar ' ' . showsPrec 11 y

boundRenderbuffer :: RenderbufferTarget -> StateVar (Renderbuffer a)
boundRenderbuffer (RenderbufferTargeting target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Renderbuffer (fromIntegral i)
  s = glBindRenderbuffer target . coerce
