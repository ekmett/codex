{-# language CPP #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language PatternSynonyms #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language DeriveGeneric #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL Attributes
module Graphics.Glow.Attribute
  ( 
  -- * Layout
    Layout(..)
  , BaseType, Components, Normalized, Stride, OffsetPtr
  -- * Attributes
  , Attribute(..)
  , AttributeLocation
  , attributeLocation
  -- * Attribute Layout Definition
  , vertexAttribute
  , setVertexAttribute
  , vertexAttribPointerI
  , vertexAttribPointer
  -- * Layout Annotation
  , UnAnnotated(..), LayoutAnnotation(..)
  , HasLayoutAnnotation(..)
  , AsType(..)
  ) where

import Control.Monad.IO.Class
import Data.Data
import Data.Functor.Contravariant
import Data.Int
import Data.StateVar
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics hiding (V1)
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear

import Graphics.Glow.Types
import Graphics.Glow.Program

--------------------------------------------------------------------------------
-- * Attribute Locations
--------------------------------------------------------------------------------

type AttributeLocation = GLuint

-- | Returns 'Nothing' if the attribute is unbound within the program.
attributeLocation :: MonadIO m => Program -> String -> m (Maybe AttributeLocation)
attributeLocation (Program p) s = liftIO $ check <$> withCString s (glGetAttribLocation p . castPtr) where
  check n
    | n < 0     = Nothing
    | otherwise = Just $ fromIntegral n

--------------------------------------------------------------------------------
-- * Layout Definition
--------------------------------------------------------------------------------

type BaseType   = GLenum
type Components = Int
type Stride     = Int
type Normalized = Bool
type OffsetPtr  = Ptr ()
data AsType     = AsInteger | AsFloating

data Layout     = Layout Components BaseType Normalized Stride OffsetPtr
  deriving (Data,Typeable,Generic,Eq,Ord,Show)

newtype UnAnnotated a = UnAnnotated { unAnnotate :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)
newtype LayoutAnnotation a = LayoutAnnotation { getLayout :: Layout }
  deriving (Data,Typeable,Generic,Eq,Ord,Show)

instance Storable a => Storable (UnAnnotated a) where
  sizeOf _ = sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  peek = fmap UnAnnotated . peek . castPtr
  poke ptr = poke (castPtr ptr) . unAnnotate

class HasLayoutAnnotation a where
  -- | Annotates any proxy 'p' of an attribute 'a' with memory 'Layout' informations 
  layoutAnnotation :: Functor p => p (a UnAnnotated) -> a LayoutAnnotation

type AttributeAccessor a b = a LayoutAnnotation -> LayoutAnnotation b

-- | Associates the vertex attribute to the data layout in the vertex buffer. 
-- The association is stored in the vertex array object 'OpenGL' wise, so a VAO must be bound
-- 
-- See 'setVertexAttribute' for enabling and disabling vertex attribute semantics
vertexAttribute :: HasLayoutAnnotation a => AttributeLocation -> SettableStateVar (Maybe (AttributeAccessor a b))
vertexAttribute = contramap (fmap (\accessor -> getLayout . accessor $ layoutAnnotation (Proxy::Proxy a))) . setVertexAttribute

-- | Associates the vertex attribute to the data layout in the vertex buffer. 
-- The association is stored in the vertex array object 'OpenGL' wise, so a VAO must be bound
-- 
-- To disable a vertex attribute set @'Nothing'@
-- 
-- @
-- setVertexAttribute location $= Nothing
-- @
-- 
-- To enable a vertex attribute set @'Just' someLayout@, for example:
-- 
-- @
-- setVertexAttribute location $= 'Just' ('Layout' 3 'GL_FLOAT' False (3 * 'sizeOf' Float) 'nullPtr')
-- @
setVertexAttribute :: AttributeLocation -> SettableStateVar (Maybe Layout)
setVertexAttribute l = SettableStateVar $ \case
  Nothing -> glDisableVertexAttribArray l
  Just layout -> do
    glEnableVertexAttribArray l
    vertexAttribPointer l layout


-- | A "not so high level" binding to 'glVertexAttribIPointer'
--
-- sets the attribute array pointer for an integer attribute (no conversion)
vertexAttribPointerI :: MonadIO m => AttributeLocation -> Layout -> m ()
vertexAttribPointerI loc (Layout comp ty _norm stride offPtr) = 
  liftIO $ glVertexAttribIPointer loc (fromIntegral comp) ty (fromIntegral stride) offPtr

-- | A "not so high level" binding to 'glVertexAttribPointer'
--
-- sets the attribute array pointer for an integer or floating attribute (integers are converted to the floating type)
vertexAttribPointer :: MonadIO m => AttributeLocation -> Layout -> m ()
vertexAttribPointer loc (Layout comp ty toNorm stride offPtr) =
  liftIO $ glVertexAttribPointer loc (fromIntegral comp) ty (if toNorm then GL_TRUE else GL_FALSE) (fromIntegral stride) offPtr

--------------------------------------------------------------------------------
-- * Attribute Definition
--------------------------------------------------------------------------------

class Attribute a where
  -- | 1, 2, 3 or 4 supported
  components :: p a -> Int
  baseType :: p a -> BaseType
  -- | specifies whether integer data values should be normalized ('True') 
  -- or converted directly as float values ('False') when they are accessed.
  -- default is 'False'
  normalize :: p a -> Bool
  normalize _ = False

instance Attribute a => Attribute (UnAnnotated a) where
  components _ = components (Proxy::Proxy a)
  baseType _ = baseType (Proxy::Proxy a)

instance Attribute Float where
  components _ = 1
  baseType _ = GL_FLOAT

instance Attribute Double where
  components _ = 1
  baseType _ = GL_DOUBLE

instance Attribute Int32 where
  components _ = 1
  baseType _ = GL_INT

instance Attribute Int16 where
  components _ = 1
  baseType _ = GL_SHORT

instance Attribute Int8 where
  components _ = 1
  baseType _ = GL_BYTE

instance Attribute Word32 where
  components _ = 1
  baseType _ = GL_UNSIGNED_INT

instance Attribute Word16 where
  components _ = 1
  baseType _ = GL_UNSIGNED_SHORT

instance Attribute Word8 where
  components _ = 1
  baseType _ = GL_UNSIGNED_BYTE

instance HasGLType a => Attribute (V1 a) where
  components _ = 1
  baseType _ = asGLType (Proxy::Proxy a)

instance HasGLType a => Attribute (V2 a) where
  components _ = 2
  baseType _ = asGLType (Proxy::Proxy a)

instance HasGLType a => Attribute (V3 a) where
  components _ = 3
  baseType _ = asGLType (Proxy::Proxy a)

instance HasGLType a => Attribute (V4 a) where
  components _ = 4
  baseType _ = asGLType (Proxy::Proxy a)
