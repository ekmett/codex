{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language PatternSynonyms #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett and Sean Chalmers
--              (c) 2014 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Glow.Shader
( Shader (..)
, ShaderType
  ( ShaderType
  , getShaderType
  , FragmentShader
  , VertexShader
  , GeometryShader
  , TessControlShader
  , TessEvaluationShader
  , ComputeShader
  )
, showShaderType
, getShader
, shaderInfoLog
, buildShaderFrom
, releaseShaderCompiler
, shaderSource
, shaderSourceLength
, createShader
, compileStatus
, shaderIsDeleted
, compileShader
, compileShaderInclude
, buildNamedStrings
) where

import           Control.Monad            (when)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as Strict
import qualified Data.ByteString.Char8    as Char8
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Unsafe   as Strict
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Foldable            (for_)
import           Data.StateVar
import           Foreign.C.String         (CString, withCString, withCStringLen)
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Marshal          (alloca)
import           Foreign.Marshal.Array    (allocaArray, pokeArray)
import           Foreign.Ptr              (Ptr, castPtr, nullPtr, plusPtr)
import           Foreign.Storable         (peek, pokeElemOff)
import           Graphics.GL
import           Graphics.GL.Ext.ARB.ShadingLanguageInclude
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

import           Graphics.Glow.Object

newtype ShaderType = ShaderType { getShaderType :: GLenum }

pattern FragmentShader :: ShaderType
pattern FragmentShader = ShaderType GL_FRAGMENT_SHADER

pattern VertexShader :: ShaderType
pattern VertexShader = ShaderType GL_VERTEX_SHADER

pattern GeometryShader :: ShaderType
pattern GeometryShader = ShaderType GL_GEOMETRY_SHADER

-- | OpenGL 4.0 or ARB_tessellation_shader required
pattern TessControlShader :: ShaderType
pattern TessControlShader = ShaderType GL_TESS_CONTROL_SHADER

-- | OpenGL 4.0 or ARB_tessellation_shader required
pattern TessEvaluationShader :: ShaderType
pattern TessEvaluationShader = ShaderType GL_TESS_EVALUATION_SHADER

-- | OpenGL 4.3 or ARB_compute_shader required
pattern ComputeShader :: ShaderType
pattern ComputeShader = ShaderType GL_COMPUTE_SHADER

showShaderType :: ShaderType -> String
showShaderType FragmentShader = "fragment"
showShaderType VertexShader = "vertex"
showShaderType GeometryShader = "geometry"
showShaderType TessControlShader = "tesselation control"
showShaderType TessEvaluationShader = "tesselation evaluation"
showShaderType ComputeShader = "compute"
showShaderType (ShaderType t) = "shader type " ++ show t

instance Show ShaderType where
  showsPrec d = \case
    FragmentShader -> showString "FragmentShader"
    VertexShader -> showString "VertexShader"
    GeometryShader -> showString "GeometryShader"
    TessControlShader -> showString "TessControlShader"
    TessEvaluationShader -> showString "TessEvaluationShader"
    ComputeShader -> showString "ComputeShader"
    ShaderType t -> showParen (d > 10) $ showString "ShaderType " . showsPrec 11 t
    
newtype Shader = Shader { shaderId :: GLuint } deriving Show

instance Object Shader where
  object (Shader s) = s
  isa (Shader s) = (GL_FALSE /=) <$> glIsShader s
  delete (Shader s) = glDeleteShader s

getShader :: MonadIO m => Shader -> GLenum -> m GLint
getShader s p = liftIO $ alloca $ \q -> glGetShaderiv (shaderId s) p q >> peek q

shaderInfoLog :: MonadIO m => Shader -> m ByteString
shaderInfoLog s = do
  l <- shaderInfoLogLength s
  if l <= 1
    then return Strict.empty
    else liftIO $ alloca $ \pl ->
      Strict.createUptoN l $ \ps -> do
        glGetShaderInfoLog (shaderId s) (fromIntegral l) pl (castPtr ps)
        return $ l - 1

withCStrings :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStrings all_xs f = go 0 [] all_xs where
  go !n acc (x:xs) = withCString x $ \s -> go (n + 1) (s:acc) xs
  go !n acc [] = allocaArray n $ \p -> do
    pokeArray p (reverse acc)
    f n p

releaseShaderCompiler :: MonadIO m => m ()
releaseShaderCompiler = glReleaseShaderCompiler

compileShader :: MonadIO m => Shader -> m ()
compileShader (Shader s) = glCompileShader s

-- | Create a shader
createShader :: MonadIO m => ShaderType -> m Shader
createShader = fmap Shader . glCreateShader . getShaderType

-- | Check if the shader is deleted in OpenGL
shaderIsDeleted :: MonadIO m => Shader -> m Bool
shaderIsDeleted s = (GL_TRUE ==) <$> getShader s GL_DELETE_STATUS
--
-- -- | Returns true if the last compileShader action on this shader completed successfully.
compileStatus :: MonadIO m => Shader -> m Bool
compileStatus s = (GL_TRUE ==) <$> getShader s GL_COMPILE_STATUS
--
-- -- | Retrieve the length of the source code of this shader, including the null terminating character
shaderSourceLength :: MonadIO m => Shader -> m Int
shaderSourceLength s = fromIntegral <$> getShader s GL_SHADER_SOURCE_LENGTH
--
shaderInfoLogLength :: MonadIO m => Shader -> m Int
shaderInfoLogLength s = fromIntegral <$> getShader s GL_INFO_LOG_LENGTH

buildShaderFrom :: MonadIO m => ShaderType -> ByteString -> m Shader
buildShaderFrom stype src = liftIO $ do
  let
    chunks = Lazy.toChunks (Lazy.fromStrict src)

    go :: Shader -> Int -> [Strict.ByteString] -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()
    go s i (Strict.PS fp o l:cs) ps pl = do
      pokeElemOff pl i (fromIntegral l)
      withForeignPtr fp $ \p -> do
        pokeElemOff ps i (castPtr p `plusPtr` o)
        go s (i+1) cs ps pl

    go (Shader s) i [] ps pl =
      glShaderSource s (fromIntegral i) ps pl

  s <- Shader <$> glCreateShader (getShaderType stype)

  allocaArray (length chunks) $ \ps ->
    allocaArray (length chunks) $ \pl ->
      go s 0 chunks ps pl

  compileShader s

  sOK <- (GL_TRUE ==) <$> getShader s GL_COMPILE_STATUS

  if sOK then pure s else do
    slog <- shaderInfoLog s
    hPutStrLn stderr ("Error in " <> showShaderType stype <> " shader\n")
    Char8.hPutStrLn stderr slog
    exitFailure


-- | Get/set the source code to a shader.
shaderSource :: Shader -> StateVar Lazy.ByteString
shaderSource (Shader sh) = StateVar g s where
  g :: IO Lazy.ByteString
  g = alloca $ \pl -> do
    l <- glGetShaderiv sh GL_SHADER_SOURCE_LENGTH pl >> peek pl
    let l' = fromIntegral l
    if l <= 1
      then return Lazy.empty
      else do
        chunk <- Strict.createUptoN l' $ \ps -> (l'-1) <$ glGetShaderSource sh l pl (castPtr ps)
        return $ Lazy.fromChunks [chunk]

  s :: Lazy.ByteString -> IO ()
  s bs = allocaArray (length chunks) $ \ps -> allocaArray (length chunks) $ \pl -> go 0 chunks ps pl
    where chunks = Lazy.toChunks bs

  go :: Int -> [Strict.ByteString] -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()
  go i (Strict.PS fp o l:cs) ps pl = do
   pokeElemOff pl i (fromIntegral l)
   withForeignPtr fp $ \p -> do
     pokeElemOff ps i (castPtr p `plusPtr` o)
     go (i+1) cs ps pl
  go i [] ps pl = glShaderSource sh (fromIntegral i) ps pl

-- |
--
-- Using statically embedded strings in the executable:
--
-- @
-- buildNamedStrings $(embedDir "foo") ('/':)
-- @
--
-- Dynamically embedding all files in a given directory into the named string set
--
-- @
-- getDir "foo" >>= \ ss -> buildNamedStrings ss ('/':)
-- @
--
-- Falls back to doing nothing if 'gl_ARB_shading_langauge_include' isn't available.
buildNamedStrings :: MonadIO m => [(FilePath, Strict.ByteString)] -> (FilePath -> String) -> m ()
buildNamedStrings includes tweak = liftIO $ when gl_ARB_shading_language_include $
  for_ includes $ \(fp',body) ->
    withCStringLen (tweak fp') $ \ (name, namelen) ->
      Strict.unsafeUseAsCString body $ \string ->
        glNamedStringARB GL_SHADER_INCLUDE_ARB (fromIntegral namelen) name (fromIntegral $ Strict.length body) string

-- | Compile a shader with @#include@ support (if available).
--
-- Remember to use
--
-- @
-- #extension GL_ARB_shading_language_include : <behavior>
-- @
--
-- as appropriate within your shader.
--
-- Falls back to 'compileShader' if 'gl_ARB_shading_language_include' isn't available.
compileShaderInclude :: MonadIO m => Shader -> [FilePath] -> m ()
compileShaderInclude (Shader s) path
  | gl_ARB_shading_language_include =
    liftIO $ withCStrings path $ \n cpcs -> glCompileShaderIncludeARB s (fromIntegral n) cpcs nullPtr
  | otherwise = glCompileShader s
