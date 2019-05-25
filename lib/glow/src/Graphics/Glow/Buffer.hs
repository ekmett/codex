{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL Doc: <https://www.opengl.org/sdk/docs/man/html/glBindBuffer.xhtml>
--
-- Also usable for bindless rendering:
-- <https://www.opengl.org/discussion_boards/showthread.php/170388-Bindless-Stuff?p=1199088&viewfull=1#post1199088>
module Graphics.Glow.Buffer
  ( Buffer(..)
  , boundBufferAt
  , Alloca(..)
  -- * Buffer Data
  , BufferData(..)
  , bufferData
  , bufferDataDirect
  -- * Buffer Targets
  , BufferTarget
    ( BufferTarget
    , ArrayBuffer
    , AtomicCounterBuffer
    , CopyReadBuffer
    , CopyWriteBuffer
    , DispatchIndirectBuffer
    , DrawIndirectBuffer
    , ElementArrayBuffer
    , PixelPackBuffer
    , PixelUnpackBuffer
    , QueryBuffer
    , ShaderStorageBuffer
    , TextureBuffer
    , TransformFeedbackBuffer
    , UniformBuffer
    )
  -- * Buffer Usage
  -- $usage
  , BufferUsage
    ( BufferUsage
    , StreamDraw, StreamRead, StreamCopy
    , StaticDraw, StaticRead, StaticCopy
    , DynamicDraw, DynamicRead, DynamicCopy
    )
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Data.StateVar
import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Ext.EXT.DirectStateAccess
import Graphics.GL.Types

import Graphics.Glow.Object
import Graphics.Glow.Block

-- | A 'Buffer' is the generic OpenGL storage object for multiple possible kind of data
--
-- For ArrayBuffer it storages vertex attributes like position, normal or color an provides
-- the MD (Multiple Data) in SIMD (Single Instruction, Multiple Data)
newtype Buffer a = Buffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

data BufferTarget = BufferTarget GLenum GLenum deriving (Typeable,Data,Generic)

newtype BufferUsage = BufferUsage GLenum deriving (Eq,Num,Typeable,Data,Generic)

pattern StreamDraw :: BufferUsage
pattern StreamDraw = BufferUsage GL_STREAM_DRAW
pattern StreamRead :: BufferUsage
pattern StreamRead = BufferUsage GL_STREAM_READ
pattern StreamCopy :: BufferUsage
pattern StreamCopy = BufferUsage GL_STREAM_COPY

pattern StaticDraw :: BufferUsage
pattern StaticDraw = BufferUsage GL_STATIC_DRAW
pattern StaticRead :: BufferUsage
pattern StaticRead = BufferUsage GL_STATIC_READ
pattern StaticCopy :: BufferUsage
pattern StaticCopy = BufferUsage GL_STATIC_COPY

pattern DynamicDraw :: BufferUsage
pattern DynamicDraw = BufferUsage GL_DYNAMIC_DRAW
pattern DynamicRead :: BufferUsage
pattern DynamicRead = BufferUsage GL_DYNAMIC_READ
pattern DynamicCopy :: BufferUsage
pattern DynamicCopy = BufferUsage GL_DYNAMIC_COPY

instance Show BufferUsage where
  showsPrec d = \case
    StreamDraw -> showString "StreamDraw"
    StreamRead -> showString "StreamRead"
    StreamCopy -> showString "StreamCopy"
    StaticDraw -> showString "StaticDraw"
    StaticRead -> showString "StaticRead"
    StaticCopy -> showString "StaticCopy"
    DynamicDraw -> showString "DynamicDraw"
    DynamicRead -> showString "DynamicRead"
    DynamicCopy -> showString "DynamicCopy"
    BufferUsage t -> showParen (d > 10) $ showString "BufferUsage " . showsPrec 11 t

newtype BufferException = BufferException String deriving (Show,Typeable)
instance Exception BufferException

instance Object (Buffer a) where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsBuffer (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteBuffers (fromIntegral n) p
    where n = length xs

instance Gen (Buffer a) where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenBuffers (fromIntegral n) p
    map Buffer <$> peekArray n p

instance Default (Buffer a) where
  def = Buffer 0

-- * Buffer Data

class BufferData a where
  -- | perfom a monadic action with the pointer to the raw content and the number of elements
  withRawData :: a -> (Ptr () -> IO b) -> IO b
  -- | reads 'a' from a pointer and the given size of a in bytes 
  fromRawData :: Int -> Ptr () -> IO a
  -- | size of the complete data in bytes
  sizeOfData :: a -> Int

-- | This instance writes the data interleaved because the 'Vector' structure is already interleaved.
-- If you want an different layout use a newtype wrapper or an own data structure.
instance Storable a => BufferData (V.Vector a) where
  withRawData v m = V.unsafeWith v $ m . castPtr
  fromRawData bytes ptr = do
    fp <- newForeignPtr_ $ castPtr ptr
    return $ V.unsafeFromForeignPtr0 fp (bytes `div` sizeOf (undefined::a))
  sizeOfData v = V.length v * sizeOf (undefined::a)

instance Storable a => BufferData [a] where
  withRawData v m = withArray v $ m . castPtr
  fromRawData bytes = peekArray (bytes `div` sizeOf (undefined::a)) . castPtr
  sizeOfData v = length v * sizeOf (undefined::a)

-- for deriving via
newtype Alloca a = Alloca a
  deriving (Eq,Ord,Show,Read,Storable)

instance Storable a => BufferData (Alloca a) where
  withRawData a m = alloca $ \p -> poke p a *> m (castPtr p)
  fromRawData _ p = peek (castPtr p)
  sizeOfData = sizeOf
  {-# inline sizeOfData #-}

deriving via (Alloca (STD140 a)) instance Block a => BufferData (STD140 a)
deriving via (Alloca (STD430 a)) instance Block a => BufferData (STD430 a)

-- * Buffer Access

boundBufferAt :: BufferTarget -> StateVar (Buffer a)
boundBufferAt (BufferTarget target binding) = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Buffer (fromIntegral i)
  s = glBindBuffer target . coerce

-- | bindless uploading data to the argumented buffer (since OpenGL 4.4+ or with 'gl_EXT_direct_state_access')
bufferDataDirect :: forall a. BufferData a => Buffer a -> StateVar (BufferUsage, a)
bufferDataDirect (Buffer i)
  | gl_EXT_direct_state_access = StateVar g s
  | otherwise = throw $ BufferException "gl_EXT_direct_state_access unsupported" where
  g = alloca $ \sizePtr ->
      alloca $ \usagePtr -> do
        glGetNamedBufferParameterivEXT i GL_BUFFER_SIZE sizePtr
        glGetNamedBufferParameterivEXT i GL_BUFFER_USAGE usagePtr
        usage <- peek usagePtr
        size  <- peek sizePtr
        allocaBytes (fromIntegral size) $ \rawPtr -> do
          glGetNamedBufferSubDataEXT i 0 (fromIntegral size) (castPtr rawPtr)
          (BufferUsage $ fromIntegral usage,) <$> fromRawData (fromIntegral size) rawPtr
  s (u,v) = withRawData v $ \ptr -> glNamedBufferDataEXT i (fromIntegral $ sizeOfData v) ptr (coerce u)

-- | uploading data to the currently at 'BufferTarget' bound buffer
bufferData :: forall a. BufferData a => BufferTarget -> StateVar (BufferUsage, a)
bufferData (BufferTarget t _) = StateVar g s where
  g = alloca $ \sizePtr ->
      alloca $ \usagePtr -> do
        glGetBufferParameteriv t GL_BUFFER_SIZE sizePtr
        glGetBufferParameteriv t GL_BUFFER_USAGE usagePtr
        usage <- BufferUsage . fromIntegral <$> peek usagePtr
        size  <- peek sizePtr
        allocaBytes (fromIntegral size) $ \rawPtr -> do
          glGetBufferSubData t 0 (fromIntegral size) (castPtr rawPtr)
          (usage,) <$> fromRawData (fromIntegral size) rawPtr
  s (u,v) = withRawData v $ \ptr -> glBufferData t (fromIntegral $ sizeOfData v) ptr (coerce u)

-- * Buffer Types

-- | Vertex attributes
pattern ArrayBuffer :: BufferTarget
pattern ArrayBuffer = BufferTarget GL_ARRAY_BUFFER GL_ARRAY_BUFFER_BINDING

-- | Atomic counter storage
pattern AtomicCounterBuffer :: BufferTarget
pattern AtomicCounterBuffer = BufferTarget GL_ATOMIC_COUNTER_BUFFER GL_ATOMIC_COUNTER_BUFFER_BINDING

-- | Buffer copy source
pattern CopyReadBuffer :: BufferTarget
pattern CopyReadBuffer = BufferTarget GL_COPY_READ_BUFFER GL_COPY_READ_BUFFER_BINDING

-- | Buffer copy destination
pattern CopyWriteBuffer :: BufferTarget
pattern CopyWriteBuffer = BufferTarget GL_COPY_WRITE_BUFFER GL_COPY_WRITE_BUFFER_BINDING

-- | Indirect compute dispatch commands
pattern DispatchIndirectBuffer :: BufferTarget
pattern DispatchIndirectBuffer = BufferTarget GL_DISPATCH_INDIRECT_BUFFER GL_DISPATCH_INDIRECT_BUFFER_BINDING

-- | Indirect command arguments
pattern DrawIndirectBuffer :: BufferTarget
pattern DrawIndirectBuffer = BufferTarget GL_DRAW_INDIRECT_BUFFER GL_DRAW_INDIRECT_BUFFER_BINDING

-- | Vertex array indices
pattern ElementArrayBuffer :: BufferTarget
pattern ElementArrayBuffer = BufferTarget GL_ELEMENT_ARRAY_BUFFER GL_ELEMENT_ARRAY_BUFFER_BINDING

-- | Pixel read target
pattern PixelPackBuffer :: BufferTarget
pattern PixelPackBuffer = BufferTarget GL_PIXEL_PACK_BUFFER GL_PIXEL_PACK_BUFFER_BINDING

-- | Texture data source
pattern PixelUnpackBuffer :: BufferTarget
pattern PixelUnpackBuffer = BufferTarget GL_PIXEL_UNPACK_BUFFER GL_PIXEL_UNPACK_BUFFER_BINDING

-- | Query result buffer
pattern QueryBuffer :: BufferTarget
pattern QueryBuffer = BufferTarget GL_QUERY_BUFFER GL_QUERY_BUFFER_BINDING

-- | Shader storage buffers
--
-- You should probably use the 'Graphics.Glow.Block.STD140' or 'Graphics.Glow.Block.STD430' newtype wrapper around the contents.
-- Requires OpenGL 4.3+
pattern ShaderStorageBuffer :: BufferTarget
pattern ShaderStorageBuffer = BufferTarget GL_SHADER_STORAGE_BUFFER GL_SHADER_STORAGE_BUFFER_BINDING

-- | Texture data buffer
pattern TextureBuffer :: BufferTarget
pattern TextureBuffer = BufferTarget GL_TEXTURE_BUFFER GL_TEXTURE_BUFFER_BINDING

-- | Transform feedback buffer
pattern TransformFeedbackBuffer :: BufferTarget
pattern TransformFeedbackBuffer = BufferTarget GL_TRANSFORM_FEEDBACK_BUFFER GL_TRANSFORM_FEEDBACK_BUFFER_BINDING

-- | Uniform block storage
--
-- You should probably use the 'Graphics.Glow.Block.STD140' newtype wrapper around the contents.
pattern UniformBuffer :: BufferTarget
pattern UniformBuffer = BufferTarget GL_UNIFORM_BUFFER GL_UNIFORM_BUFFER_BINDING

instance Show BufferTarget where
  showsPrec d = \case
    ArrayBuffer -> showString "ArrayBuffer"
    AtomicCounterBuffer -> showString "AtomicCounterBuffer"
    CopyReadBuffer -> showString "CopyReadBuffer"
    CopyWriteBuffer -> showString "CopyWriteBuffer"
    DispatchIndirectBuffer -> showString "DispatchIndirectBuffer"
    DrawIndirectBuffer -> showString "DrawIndirectBuffer"
    ElementArrayBuffer -> showString "ElementArrayBuffer"
    PixelPackBuffer -> showString "PixelPackBuffer"
    PixelUnpackBuffer -> showString "PixelUnpackBuffer"
    QueryBuffer -> showString "QueryBuffer"
    ShaderStorageBuffer -> showString "ShaderStorageBuffer"
    TextureBuffer -> showString "TextureBuffer"
    TransformFeedbackBuffer -> showString "TransformFeedbackBuffer"
    UniformBuffer -> showString "UniformBuffer"
    BufferTarget x y -> showParen (d > 10) $
      showString "BufferTarget " . showsPrec 11 x . showChar ' ' . showsPrec 11 y

-- * Usage

-- $usage
--
-- Terminology:
--
-- [Stream] The data store contents will be modified once and used at most a few times.
--
-- [Static] The data store contents will be modified once and used many times.
--
-- [Dynamic] The data store contents will be modified repeatedly and used many times.
--
-- [Draw] The data store contents are modified by the application, and used as the source for GL drawing and image specification commands.
--
-- [Read] The data store contents are modified by reading data from the GL, and used to return that data when queried by the application.
--
-- [Copy] The data store contents are modified by reading data from the GL, and used as the source for GL drawing and image specification commands.

