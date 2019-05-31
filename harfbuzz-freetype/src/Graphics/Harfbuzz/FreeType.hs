{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
module Graphics.Harfbuzz.FreeType
( hb_ft_face_create_referenced
, hb_ft_font_create_referenced
, hb_ft_font_get_face
, hb_ft_font_load_flags -- statevar
, hb_ft_font_changed
, hb_ft_font_set_funcs
) where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Map as Map
import Data.StateVar (StateVar(..))
import qualified Foreign.Concurrent as Concurrent
import Foreign.Ptr (Ptr, nullPtr)
import qualified Graphics.FreeType.Internal as FT
import qualified Graphics.Harfbuzz.Internal as HB
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

-- TODO freetypeCtx
C.context $ C.baseCtx <> C.fptrCtx <> HB.harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Face", [t|Ptr FT.FaceRec|])
    , (C.TypeName "FT_Error", [t|FT.Error|])
    ]
  }

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
    FT_Face face = hb_ft_font_get_face($font:font);
    if (face) FT_Reference_Face(face);
    return face;
  }|]
  if p == nullPtr then return Nothing
  else Just . FT.Face <$> Concurrent.newForeignPtr p ([C.exp|FT_Error { FT_Done_Face($(FT_Face p)) }|] >>= FT.ok)

hb_ft_font_load_flags :: HB.Font -> StateVar Int
hb_ft_font_load_flags font = StateVar g s where
  g = fromIntegral <$> [C.exp|int { hb_ft_font_get_load_flags($font:font) }|]
  s (fromIntegral -> v) = [C.block|void { hb_ft_font_set_load_flags($font:font,$(int v)); }|]

hb_ft_font_changed :: MonadIO m => HB.Font -> m ()
hb_ft_font_changed font = liftIO [C.block|void { hb_ft_font_changed($font:font); }|]

hb_ft_font_set_funcs :: MonadIO m => HB.Font -> m ()
hb_ft_font_set_funcs font = liftIO [C.block|void { hb_ft_font_set_funcs($font:font); }|]
