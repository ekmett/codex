{-# language DeriveDataTypeable #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Glow.VertexArray
( VertexArray(..)
, boundVertexArray
, withVertexArray
) where

import Control.Exception (bracket)
import Control.Applicative
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types

import Graphics.Glow.Object

newtype VertexArray = VertexArray GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object VertexArray where
  object = coerce
  isa i = (GL_FALSE /=) <$> glIsVertexArray (coerce i)
  deletes xs = liftIO $ allocaArray n \p -> do
    pokeArray p (coerce xs)
    glDeleteVertexArrays (fromIntegral n) p
    where n = length xs

instance Gen VertexArray where
  gens n = liftIO $ allocaArray n \p -> do
    glGenVertexArrays (fromIntegral n) p
    map VertexArray <$> peekArray n p

instance Default VertexArray where
  def = VertexArray 0

boundVertexArray :: StateVar VertexArray
boundVertexArray = StateVar g s where
  g = do
    i <- alloca $ liftA2 (>>) (glGetIntegerv GL_VERTEX_ARRAY_BINDING) peek
    return $ VertexArray (fromIntegral i)
  s = glBindVertexArray . coerce

withVertexArray :: VertexArray -> IO r -> IO r
withVertexArray vao = bracket (get boundVertexArray <* do boundVertexArray $= vao) (boundVertexArray $=) . const
