{-# language BlockArguments #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett and Sean Chalmers
--              (c) 2014 Edward Kmett 
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Glow.Program
( Program(..)
, buildProgram
-- * Attachment
, attachShader
, detachShader
, numAttachedShaders
, attachedShaders
-- * Linking
, linkProgram
, linkStatus
-- * Status
, programInfoLog
, programIsDeleted
, numActiveAttributes
, activeAttributeMaxLength
, numActiveUniforms
, activeUniformMaxLength
, activeAtomicCounterBuffers
, programBinaryLength
, programComputeWorkGroupSize
, transformFeedbackVaryingsMaxLength
, transformFeedbackBufferMode
, numTransformFeedbackVaryings
, geometryVerticesOut
, geometryInputType
, geometryOutputType
, currentProgram
, createShaderProgramInclude
-- * Validation
, validateStatus
, validateProgram
) where

import           Control.Applicative      (liftA2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad            (unless, when)
import qualified Data.ByteString          as Strict
import qualified Data.ByteString.Char8    as Char8
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Coerce              (coerce)
import           Data.Default
import           Data.StateVar
import           Foreign.Marshal          (alloca)
import           Foreign.Marshal.Array    (allocaArray, peekArray)
import           Foreign.Ptr              (castPtr, nullPtr)
import           Foreign.Storable         (peek, peekElemOff)
import           Graphics.GL
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

import           Graphics.Glow.Object
import           Graphics.Glow.Shader

newtype Program = Program { programId :: GLuint } deriving Show

instance Object Program where
  object = coerce
  isa p = (GL_FALSE /=) <$> glIsProgram (coerce p)
  delete p = glDeleteProgram (coerce p)

instance Gen Program where
  gen = Program <$> glCreateProgram

instance Default Program where
  def = Program 0

attachShader :: MonadIO m => Program -> Shader -> m ()
attachShader (Program p) (Shader s) = glAttachShader p s

detachShader :: MonadIO m => Program -> Shader -> m ()
detachShader (Program p) (Shader s) = glDetachShader p s

-- @'numAttachedShaders' program@ returns the number of shader objects attached to @program@.
numAttachedShaders :: MonadIO m => Program -> m Int
numAttachedShaders p = liftIO $ fromIntegral <$> get (programParameter1 p GL_ATTACHED_SHADERS)

attachedShaders :: MonadIO m => Program -> m [Shader]
attachedShaders p = liftIO do
  numShaders <- numAttachedShaders p
  ids <- allocaArray numShaders \buf -> do
    glGetAttachedShaders (object p) (fromIntegral numShaders) nullPtr buf
    peekArray numShaders buf
  return $ map Shader ids

linkProgram :: MonadIO m => Program -> m ()
linkProgram = glLinkProgram . object

buildProgram :: MonadIO m => Shader -> Shader -> m Program
buildProgram vs fs = liftIO do
  program <- glCreateProgram
  let p = Program program

  attachShader p vs
  attachShader p fs

  glLinkProgram program

  linkOK <- linkStatus p
  glValidateProgram program

  status <- validateStatus p

  unless (linkOK && status) do
    hPutStrLn stderr "GL.linkProgram error"
    plog <- programInfoLog p
    Char8.hPutStrLn stderr plog
    exitFailure

  p <$ glUseProgram program

programParameter1 :: Program -> GLenum -> StateVar GLint
programParameter1 p parm = StateVar g s where
  g = alloca $ liftA2 (>>) (glGetProgramiv (coerce p) parm) peek
  s = glProgramParameteri (coerce p) parm

linkStatus :: MonadIO m => Program -> m Bool
linkStatus p = (GL_FALSE /=) <$> get (programParameter1 p GL_LINK_STATUS)

programInfoLog :: MonadIO m => Program -> m Strict.ByteString
programInfoLog p = liftIO do
  l <- fromIntegral <$> get (programParameter1 p GL_INFO_LOG_LENGTH)
  if l <= 1
    then return Strict.empty
    else liftIO $ alloca \pl ->
      Strict.createUptoN l \ps -> do
        glGetProgramInfoLog (object p) (fromIntegral l) pl (castPtr ps)
        return $ l-1

-- | @'programIsDeleted' program@ returns 'True' if @program@ is currently flagged for deletion, 'False' otherwise.
programIsDeleted :: MonadIO m => Program -> m Bool
programIsDeleted p = (GL_FALSE /=) <$> get (programParameter1 p GL_DELETE_STATUS)

-- | Check if the shader is a separable program
-- separable shader programs can be created by 'createShaderProgram'
programSeparable :: Program -> StateVar Bool
programSeparable p = mapStateVar toGLBool fromGLBool $ programParameter1 p GL_PROGRAM_SEPARABLE where
  toGLBool b = if b then GL_TRUE else GL_FALSE
  fromGLBool = (GL_TRUE ==)

-- * Validation

-- | @'validateProgram' program@ checks to see whether the executables contained in @program@ can
-- execute given the current OpenGL state. The information generated by the validation process
-- will be stored in @program@'s information log. The validation information may consist of an
-- empty string, or it may be a string containing information about how the current program object
-- interacts with the rest of current OpenGL state. This provides a way for OpenGL implementers to
-- convey more information about why the current program is inefficient, suboptimal, failing to execute, and so on.
validateProgram :: MonadIO m => Program -> m ()
validateProgram (Program p) = glValidateProgram p

validateStatus :: MonadIO m => Program -> m Bool
validateStatus p = (GL_FALSE /=) <$> get (programParameter1 p GL_VALIDATE_STATUS)

-- * Atomic Counter Buffers

-- | @'activeAtomicCounterBuffers' program@ returns the number of active attribute atomic counter buffers used by @program@.
activeAtomicCounterBuffers :: MonadIO m => Program -> m Int
activeAtomicCounterBuffers p = fromIntegral <$> get (programParameter1 p GL_ACTIVE_ATOMIC_COUNTER_BUFFERS)

-- * Attributes

-- | @'numActiveAttributes' program@ returns the number of active attribute variables for @program@.
numActiveAttributes :: MonadIO m => Program -> m Int
numActiveAttributes p = fromIntegral <$> get (programParameter1 p GL_ACTIVE_ATTRIBUTES)

-- | @'activeAttributeMaxLength' program@  returns the length of the longest active attribute name for @program@, including the null termination character (i.e., the size of the character buffer required to store the longest attribute name). If no active attributes exist, 0 is returned.
activeAttributeMaxLength :: MonadIO m => Program -> m Int
activeAttributeMaxLength p = fromIntegral <$> get (programParameter1 p GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)

-- * Uniforms

-- | @'numActiveUniforms' returns the number of active uniform variables for @program@.
numActiveUniforms :: MonadIO m => Program -> m Int
numActiveUniforms p = fromIntegral <$> get (programParameter1 p GL_ACTIVE_UNIFORMS)

-- | @'activeUniformMaxLength' program@  returns the length of the longest active uniform variable name for @program@, including the null termination character (i.e., the size of the character buffer required to store the longest uniform variable name). If no active uniform variables exist, 0 is returned.
activeUniformMaxLength :: MonadIO m => Program -> m Int
activeUniformMaxLength p = fromIntegral <$> get (programParameter1 p GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)

-- * Binary

-- | @'programBinaryLength' program@ return the length of the program binary, in bytes, that will be returned by a call to @glGetProgramBinary@. When a progam's @linkStatus@ is False, its program binary length is 0.
programBinaryLength :: MonadIO m => Program -> m Int
programBinaryLength p = fromIntegral <$> get (programParameter1 p GL_PROGRAM_BINARY_LENGTH)


-- * Compute Workgroups

-- | @'programComputeWorkgroupSize' program@ returns three integers containing the local work group size of the compute program as specified by its input layout qualifier(s). @program@ must be the name of a program object that has been previously linked successfully and contains a binary for the compute shader stage.
programComputeWorkGroupSize :: MonadIO m => Program -> m (Int, Int, Int)
programComputeWorkGroupSize (Program p) = liftIO $ allocaArray 3 \q -> do
  glGetProgramiv p GL_COMPUTE_WORK_GROUP_SIZE q
  a <- peek q
  b <- peekElemOff q 1
  c <- peekElemOff q 2
  return (fromIntegral a, fromIntegral b, fromIntegral c)

-- * Transform Feedback

-- | @'transformFeedbackBufferMode' program@ returns a symbolic constant indicating the buffer mode for @program@ used when transform feedback is active. This may be 'GL_SEPARATE_ATTRIBS' or 'GL_INTERLEAVED_ATTRIBS'.
transformFeedbackBufferMode :: MonadIO m => Program -> m GLenum
transformFeedbackBufferMode p = fromIntegral <$> get (programParameter1 p GL_TRANSFORM_FEEDBACK_BUFFER_MODE)

-- | @'numTransformFeedbackVaryings' program@ returns the number of varying variables to capture in transform feedback mode for the @program@.
numTransformFeedbackVaryings :: MonadIO m => Program -> m Int
numTransformFeedbackVaryings p = fromIntegral <$> get (programParameter1 p GL_TRANSFORM_FEEDBACK_VARYINGS)

-- | @'transformFeedbackVaryingsMaxLength' program@ returns the length of the longest variable name to be used for transform feedback, including the null-terminator.
transformFeedbackVaryingsMaxLength :: MonadIO m => Program -> m Int
transformFeedbackVaryingsMaxLength p = fromIntegral <$> get (programParameter1 p GL_TRANSFORM_FEEDBACK_VARYINGS)

-- * Geometry Shaders

-- | @'geometryVerticesOut' program@ returns the maximum number of vertices that the geometry shader in @program@ will output.
geometryVerticesOut :: MonadIO m => Program -> m Int
geometryVerticesOut p = fromIntegral <$> get (programParameter1 p GL_GEOMETRY_VERTICES_OUT)


-- | @'geometryInputType' program@ returns a symbolic constant indicating the primitive type accepted as input to the geometry shader contained in @program@.
geometryInputType :: MonadIO m => Program -> m GLenum
geometryInputType p = fromIntegral <$> get (programParameter1 p GL_GEOMETRY_INPUT_TYPE)

-- | @'geometryOutputType' program@ returns a symbolic constant indicating the primitive type that will be output by the geometry shader contained in @program@.
geometryOutputType :: MonadIO m => Program -> m GLenum
geometryOutputType p = fromIntegral <$> get (programParameter1 p GL_GEOMETRY_OUTPUT_TYPE)

currentProgram :: StateVar Program
currentProgram = StateVar
  (fmap (Program . fromIntegral) $ alloca $ liftA2 (>>) (glGetIntegerv GL_CURRENT_PROGRAM) peek)
  (glUseProgram . object)

-- * Separable Program

-- | @'createSeparableProgram' shaderType source paths@ emulates the missing OpenGL functionality to
-- create a separable 'Program' from source with 'glCreateShaderProgram' but
-- with 'GL_ARB_shading_language_include' support.
createShaderProgramInclude :: MonadIO m => ShaderType -> Lazy.ByteString -> [FilePath] -> m Program
createShaderProgramInclude shaderType source paths = liftIO do
  s <- createShader shaderType
  shaderSource s $= source
  compileShaderInclude s paths
  compiled <- compileStatus s
  prog <- gen
  when compiled do
    programSeparable prog $= True
    attachShader prog s
    linkProgram prog
    detachShader prog s
  delete s
  return prog
