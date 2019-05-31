{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.FreeType
( hb_ft_face_create_referenced
, hb_ft_font_create_referenced
, hb_ft_font_get_face
, hb_ft_font_load_flags -- statevar
, hb_ft_font_changed
, hb_ft_font_set_funcs
) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.StateVar (StateVar(..))
import Foreign.Ptr (nullPtr)
import qualified Graphics.FreeType.Internal as FT
import qualified Graphics.Harfbuzz.Internal as HB
import qualified Language.C.Inline as C

-- TODO freetypeCtx
C.context $ C.baseCtx <> C.fptrCtx <> HB.harfbuzzCtx <> FT.freeTypeCtx
C.include "<hb-ft.h>"
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"

hb_ft_face_create_referenced :: MonadIO m => FT.Face -> m HB.Face
hb_ft_face_create_referenced ftface = liftIO $ [C.exp|hb_face_t * { hb_ft_face_create_referenced($fptr-ptr:(FT_Face ftface)) }|] >>= HB.foreignFace

hb_ft_font_create_referenced :: MonadIO m => FT.Face -> m HB.Font
hb_ft_font_create_referenced ftface = liftIO $ [C.exp|hb_font_t * { hb_ft_font_create_referenced($fptr-ptr:(FT_Face ftface)) }|] >>= HB.foreignFont

hb_ft_font_get_face :: MonadIO m => HB.Font -> m (Maybe FT.Face)
hb_ft_font_get_face font = liftIO $ do
  p <- [C.block|FT_Face {
    FT_Face face = hb_ft_font_get_face($fptr-ptr:(hb_font_t * font));
    if (face) FT_Reference_Face(face);
    return face;
  }|]
  if p == nullPtr then return Nothing
  else Just <$> FT.foreignFace p

hb_ft_font_load_flags :: HB.Font -> StateVar Int
hb_ft_font_load_flags font = StateVar g s where
  g = fromIntegral <$> [C.exp|int { hb_ft_font_get_load_flags($fptr-ptr:(hb_font_t * font)) }|]
  s (fromIntegral -> v) = [C.block|void { hb_ft_font_set_load_flags($fptr-ptr:(hb_font_t * font),$(int v)); }|]

hb_ft_font_changed :: MonadIO m => HB.Font -> m ()
hb_ft_font_changed font = liftIO [C.block|void { hb_ft_font_changed($fptr-ptr:(hb_font_t * font)); }|]

hb_ft_font_set_funcs :: MonadIO m => HB.Font -> m ()
hb_ft_font_set_funcs font = liftIO [C.block|void { hb_ft_font_set_funcs($fptr-ptr:(hb_font_t * font)); }|]