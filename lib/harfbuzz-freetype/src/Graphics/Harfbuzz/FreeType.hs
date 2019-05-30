module Graphics.Harfbuzz.FreeType
( face_create_referenced
, font_create_referenced
, font_get_face
, font_load_flags -- statevar
, font_changed
, font_set_funcs
) where

import qualified Language.C.Inline as C

import Graphics.FreeType.Types as FreeType
import Graphics.Harfbuzz.Internal as Harfbuzz

-- TODO freetypeCtx
let anti hsTyQ w = 
 in C.context $ C.baseCtx <> harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Face", [t|Ptr FreeType.Face|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ftface", C.SomeAntiQuoter C.AntiQuoter
        { C.aqParser = C.parseIdentifier <&> \hId -> (C.mangleHaskellIdentifier hId, C.TypeSpecifier mempty $ C.TypeName "FT_Face", hId)
        , C.aqMarshaller = \_ _ _ cId -> (,) <$> [t|Ptr FreeType.Face|] <*> [|withSelf (coerce $(getHsVariable "Graphics.Harfbuzz.FreeType" cId))|]
      })
    ]
  } where

face_create_referenced :: MonadIO m => FreeType.Face -> m Harfbuzz.Face
face_create_referenced ftface = liftIO $ [C.exp|hb_face_t * { hb_ft_face_create_referenced($ftface:ftface) }|] >>= Harfbuzz.foreignFace

font_create_referenced :: MonadIO m => FreeType.Face -> m Harfbuzz.Font
font_create_referenced ftface = liftIO $ [C.exp|hb_face_t * { hb_ft_face_create_referenced($ftface:ftface) }|] >>= Harfbuzz.foreignFace

font_get_face :: MonadIO m => Harfbuzz.Font -> m (Maybe FreeType.Face)
font_get_face font = [C.exp|FT_Face { hb_ft_font_get_face($font:font)
