{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language TemplateHaskell #-}

module UI.Text.Buffer
  ( 
  -- * Text Buffers
    TextBuffer(..)
  , new, delete, clear

  -- * Pen based operations
  , Pen(..), HasPen(..), add

  -- ** Alignment
  , Align(..), align

  -- ** Computing bounding boxes
  , BoundingBox(..), boundingBox

  -- * Rendering
  ,  TextProgram, compileTextProgram, deleteTextProgram
  , render

  -- * Rendering with reduced state changes
  , withTextProgram, render' 
  ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.TextureAtlas as TA
import qualified Bindings.FreetypeGL.VertexBuffer as VB
import qualified Bindings.FreetypeGL.Vec234 as Vec234
import Control.Exception
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Default
import Data.StateVar
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Error (throwIfNull)
import Foreign.Ptr
import Foreign.Storable
import GHC.Arr
import GHC.Generics
import Graphics.GL
import Linear (identity, V3(..))

import UI.Shaders
import UI.Text.Atlas (Atlas)
import qualified UI.Text.Atlas as Atlas
import UI.Text.Font (Font)
import UI.Text.Markup (Markup)
import qualified UI.Text.Markup as Markup
import qualified Glow

data FontUniforms = FontUniforms
  { pixel :: {-# unpack #-} !Glow.Vec3
  , pvm :: {-# unpack #-} !Glow.Mat4
  }
  deriving (Show, Generic, Glow.Block)
  deriving (Storable, Glow.BufferData) via (Glow.STD140 FontUniforms)

data TextProgram = TextProgram 
  { tpId :: {-# unpack #-} !Glow.Program 
  , tpUbo :: {-# unpack #-} !(Glow.Buffer FontUniforms)
  , tpBlend1, tpBlend2 :: {-# unpack #-} !GLuint
  } deriving Show

-- wire up a program pipeline and shader the vertex shader?

-- build a generic rendering program
compileTextProgram :: MonadIO m => m TextProgram
compileTextProgram = liftIO $ do
  vs <- shader "text.vert" >>= Glow.buildShaderFrom Glow.VertexShader
  fs <- shader "text.frag" >>= Glow.buildShaderFrom Glow.FragmentShader

  p <- Glow.buildProgram vs fs
  bi <- withCString "Font" $ \ font -> glGetUniformBlockIndex (Glow.object p) font
  glUniformBlockBinding (Glow.object p) bi 0 -- tie this off as uniform block binding 0
  ubo <- Glow.gen
  TextProgram p ubo GL_ONE GL_ONE_MINUS_SRC_ALPHA <$ Glow.throwErrors

deleteTextProgram :: MonadIO m => TextProgram -> m ()
deleteTextProgram (TextProgram p ubo _ _) = liftIO $ do
  Glow.delete p
  Glow.delete ubo

newtype TextBuffer = TextBuffer (Ptr TB.C'text_buffer_t)

data Pen = Pen { _px :: {-# unpack #-} !Float, _py :: {-# unpack #-} !Float } deriving (Eq,Ord,Read,Show)

makeClassy ''Pen

-- building text buffers

new :: MonadIO m => m TextBuffer
new = liftIO $ TextBuffer <$> throwIfNull "text_buffer_new failed" TB.c'text_buffer_new

delete :: MonadIO m => TextBuffer -> m ()
delete (TextBuffer p) = liftIO $ TB.c'text_buffer_delete p

toVec :: Iso' Pen Vec234.C'vec2
toVec = iso yon hither where
  hither (Vec234.C'vec2 x y) = Pen (realToFrac x) (realToFrac y)
  yon (Pen x y) = Vec234.C'vec2 (realToFrac x) (realToFrac y)

withPen :: (MonadIO m, MonadState s m, HasPen s) => (Ptr Vec234.C'vec2 -> IO a) -> m a
withPen act = do
  oldPen <- use (pen.toVec)
  (r, newPen) <- liftIO $ alloca $ \penPtr -> do
    poke penPtr oldPen
    (,) <$> act penPtr <*> peek penPtr
  r <$ (pen.toVec .= newPen)

add :: (MonadIO m, MonadState s m, HasPen s) => TextBuffer -> Markup -> Font -> Text -> m ()
add (TextBuffer ptr) markup font text = unless (Text.null text) $ do
  withPen $ \penPtr -> Markup.withMarkupPtr markup font $ \ markupPtr -> 
    Text.withCStringLen text $ \(chars, _len) -> 
      TB.c'text_buffer_add_text ptr penPtr markupPtr chars (fromIntegral (Text.length text))

clear :: MonadIO m => TextBuffer -> m ()
clear (TextBuffer ptr) = liftIO $ TB.c'text_buffer_clear ptr

-- alignment

data Align = AlignLeft | AlignCenter | AlignRight
  deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded)

c'Align :: Align -> CUInt
c'Align AlignLeft   = TB.c'ALIGN_LEFT
c'Align AlignCenter = TB.c'ALIGN_CENTER
c'Align AlignRight  = TB.c'ALIGN_RIGHT

align :: (MonadIO m, MonadState s m, HasPen s) => TextBuffer -> Align -> m ()
align (TextBuffer p) a = withPen $ \penPtr -> TB.c'text_buffer_align p penPtr (c'Align a)

data BoundingBox = BoundingBox
  { bbLeft, bbTop, bbWidth, bbHeight :: {-# unpack #-} !Float
  } deriving (Eq, Ord, Read, Show)

boundingBox :: (MonadIO m, MonadState s m, HasPen s) => TextBuffer -> m BoundingBox
boundingBox (TextBuffer ptr) = withPen $ \penPtr -> alloca $ \boundsVec4 -> do
  TB.c'wrapper__text_buffer_get_bounds boundsVec4 ptr penPtr
  Vec234.C'vec4 left top width height <- peek boundsVec4
  return (BoundingBox (realToFrac left) (realToFrac top) (realToFrac width) (realToFrac height))

withTextProgram :: TextProgram -> Atlas -> IO r -> IO r
withTextProgram (TextProgram p ubo blend1 blend2) atlas = bracket setup teardown . const where
  setup = do
    Glow.currentProgram $= p
    glBlendFunc blend1 blend2
    glBlendColor 1 1 1 1
    -- bind the atlas
    glActiveTexture GL_TEXTURE0
    Glow.programUniform1i p 0 $= GL_TEXTURE0 -- point sampler at GL_TEXTURE0
    Glow.Texture t <- Atlas.getTexture atlas
    glBindTexture GL_TEXTURE_2D t

    -- set up uniforms
    TA.C'texture_atlas_t width height depth _ _ _ <- peek (Atlas.ptr atlas)
    let uf = FontUniforms (V3 (1/fromIntegral width) (1/fromIntegral height) (fromIntegral depth)) identity
    Glow.boundBufferAt Glow.UniformBuffer $= ubo
    Glow.bufferData Glow.UniformBuffer $= (GL_STREAM_DRAW, uf) 

  teardown _ = do
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA -- restore default application state
    Glow.currentProgram $= def

-- drawing text buffers

render :: MonadIO m => TextProgram -> Atlas -> TextBuffer -> m ()
render p a b = liftIO $ withTextProgram p a $ render' b

-- for use only within the scope of withTextProgram
render' :: MonadIO m => TextBuffer -> m ()
render' (TextBuffer ptr) = liftIO $ do
  buffer <- peek (TB.p'text_buffer_t'buffer ptr)
  VB.c'vertex_buffer_render buffer GL_TRIANGLES


