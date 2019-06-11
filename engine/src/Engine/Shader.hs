module Engine.Shader
(
) where

import Engine.Include
import Glow.Shader as G

namedString :: GivenIncludeCache => FilePath -> m (Thunk ())
namedString = 

-- create a shader program
shader :: (GivenIncludeCache, MonadIO m) => ShaderType -> [ByteString] -> m (ThunkIO ShaderProgram)
shader shaderType source = do
  body <- absolves source
  self <- GL.createShader ShaderType
  delay $ do
    -- time to recompile
    includes <- force

    iforOf_ cache $ \ path body -> do
      content <- readWatchedFile path
      glNamedStringARB GL_SHADER_INCLUDE_ARB ...
    
    -- first things first
    shader <- G.createShader
  
    len = body
    glShaderSource (coerce shader) strings lengths
