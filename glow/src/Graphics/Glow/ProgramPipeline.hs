{-# language DeriveDataTypeable #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Separable 'Programs's can be combined to a 'ProgramPipeline'
-- Requires: 4.1+ or GL_ARB_separate_shader_objects
module Graphics.Glow.ProgramPipeline
( ProgramPipeline
, PipelineStage
, boundProgramPipeline
, useProgramStages
-- * Properties
, programPipelineParameter1
, activeShaderProgram
, programPipelineInfoLog
, validateProgramPipeline
, validateProgramPipelineStatus
-- * Stages
, vertexShader
, fragmentShader
, tessControlShader
, tessEvaluationShader
, geometryShader
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.StateVar
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as Strict

import Graphics.Glow.Object
import Graphics.Glow.Program

-- | A Pipeline object captures shader stages
newtype ProgramPipeline = ProgramPipeline GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

type PipelineStage = GLbitfield

instance Object ProgramPipeline where
  object = coerce
  isa i = (GL_FALSE /=) <$> glIsProgramPipeline (coerce i)
  deletes xs = liftIO $ allocaArray n \p -> do
    pokeArray p (coerce xs)
    glDeleteProgramPipelines (fromIntegral n) p
    where n = length xs

instance Gen ProgramPipeline where
  gens n = liftIO $ allocaArray n \p -> do
    glGenProgramPipelines (fromIntegral n) p
    map ProgramPipeline <$> peekArray n p

instance Default ProgramPipeline where
  def = ProgramPipeline 0

-- * Properties

programPipelineParameter1 :: ProgramPipeline -> GLenum -> GettableStateVar GLint
programPipelineParameter1 p parm = alloca $ liftA2 (>>) (glGetProgramPipelineiv (coerce p) parm) peek

activeShaderProgram :: ProgramPipeline -> StateVar (Maybe Program)
activeShaderProgram p = StateVar g s where
  g = fmap Program . checkName <$> programPipelineParameter1 p GL_ACTIVE_PROGRAM
  s = glActiveShaderProgram (coerce p) . coerce . fromMaybe def

programPipelineInfoLog :: MonadIO m => ProgramPipeline -> m Strict.ByteString
programPipelineInfoLog p = liftIO do
  l <- fromIntegral <$> get (programPipelineParameter1 p GL_INFO_LOG_LENGTH)
  if l <= 1
    then return Strict.empty
    else liftIO $ alloca \pl ->
      Strict.createUptoN l \ps ->
        (l-1) <$ glGetProgramPipelineInfoLog (object p) (fromIntegral l) pl (castPtr ps)

-- * Validation

-- | @'validateProgramPipeline' pipeline@ instructs the implementation to validate the shader executables contained in @pipeline@ against the current GL state. The implementation may use this as an opportunity to perform any internal shader modifications that may be required to ensure correct operation of the installed shaders given the current GL state.
--
validateProgramPipeline :: MonadIO m => ProgramPipeline -> m ()
validateProgramPipeline = glValidateProgramPipeline . coerce

-- | @'validateProgramPipelineStatus' pipeline@ returns 'True' if the last validation operation on @pipeline@ was successful, and 'False' otherwise.
--
-- If 'True', @pipeline@ is guaranteed to execute given the current state. Otherwise, @pipeline@ is guaranteed to not execute.
validateProgramPipelineStatus :: MonadIO m => ProgramPipeline -> m Bool
validateProgramPipelineStatus p = (GL_FALSE /=) <$> get (programPipelineParameter1 p GL_VALIDATE_STATUS)

-- * Binding

boundProgramPipeline :: StateVar ProgramPipeline
boundProgramPipeline = StateVar g s where
  g = fmap (ProgramPipeline . fromIntegral) $ alloca $ liftA2 (>>) (glGetIntegerv GL_PROGRAM_PIPELINE_BINDING) peek
  s = glBindProgramPipeline . coerce

-- * Stages

vertexShader :: ProgramPipeline -> StateVar (Maybe Program)
vertexShader p = StateVar g s where
  g = fmap Program . checkName <$> programPipelineParameter1 p GL_VERTEX_SHADER
  s = useProgramStages p GL_VERTEX_SHADER_BIT . fromMaybe def

fragmentShader :: ProgramPipeline -> StateVar (Maybe Program)
fragmentShader p = StateVar g s where
  g = fmap Program . checkName <$> programPipelineParameter1 p GL_FRAGMENT_SHADER
  s = useProgramStages p GL_FRAGMENT_SHADER_BIT . fromMaybe def

tessControlShader :: ProgramPipeline -> StateVar (Maybe Program)
tessControlShader p = StateVar g s where
  g = fmap Program . checkName <$> programPipelineParameter1 p GL_TESS_CONTROL_SHADER
  s = useProgramStages p GL_TESS_CONTROL_SHADER_BIT . fromMaybe def

tessEvaluationShader :: ProgramPipeline -> StateVar (Maybe Program)
tessEvaluationShader p = StateVar g s where
  g = fmap Program . checkName <$> programPipelineParameter1 p GL_TESS_EVALUATION_SHADER
  s = useProgramStages p GL_TESS_EVALUATION_SHADER_BIT . fromMaybe def

geometryShader :: ProgramPipeline -> StateVar (Maybe Program)
geometryShader p = StateVar g s where
  g = fmap Program . checkName <$> programPipelineParameter1 p GL_GEOMETRY_SHADER
  s = useProgramStages p GL_GEOMETRY_SHADER_BIT . fromMaybe def

useProgramStages :: MonadIO m => ProgramPipeline -> PipelineStage -> Program -> m ()
useProgramStages pipe stage prog = glUseProgramStages (coerce pipe) stage (coerce prog)
