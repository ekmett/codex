{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.FreeType
( hb_ft_face_create
, hb_ft_face_create_cached
, hb_ft_font_create
, hb_ft_font_get_face
, hb_ft_font_load_flags -- statevar
, hb_ft_font_changed
, hb_ft_font_set_funcs
) where

import Control.Monad.ST.Unsafe
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor ((<&>))
import Data.Primitive.StateVar (StateVar(..), get)
import Foreign.ForeignPtr
import Foreign.Marshal.Unsafe (unsafeLocalState)
import GHC.Prim (RealWorld)
import GHC.Ptr
import GHC.ForeignPtr
import qualified Graphics.FreeType.Internal as FT
import qualified Graphics.Harfbuzz.Internal as HB
import qualified Graphics.Harfbuzz.Object as HB
import qualified Graphics.Harfbuzz.Font as HB
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as U

C.context $ C.baseCtx <> C.fptrCtx <> HB.harfbuzzCtx <> FT.freeTypeCtx
C.include "<hb-ft.h>"
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"

C.verbatim "static hb_user_data_key_t face_face_udk;" -- allocate a fresh key for ft_face data maintenance
C.verbatim "static hb_user_data_key_t font_face_udk;" -- allocate a fresh key for ft_face data maintenance

face_face_key :: HB.Key (Either (Ptr FT.FaceRec) FT.Face)
face_face_key = unsafeLocalState $ HB.Key <$> newForeignPtr_ [U.pure|hb_user_data_key_t * { &face_face_udk }|]

font_face_key :: HB.Key FT.Face
font_face_key = unsafeLocalState $ HB.Key <$> newForeignPtr_ [U.pure|hb_user_data_key_t * { &font_face_udk }|]

-- | Create a harfbuzz face that references our freetype face.
--
-- This will keep the freetype face alive until the harfbuzz object is destroyed.
hb_ft_face_create :: MonadIO m => FT.Face -> m (HB.Face RealWorld)
hb_ft_face_create ftface = liftIO do
  face <- [U.exp|hb_face_t * { hb_ft_face_create($fptr-ptr:(FT_Face ftface), NULL) }|] >>= HB.foreignFace
  face <$ HB.object_set_user_data face face_face_key (Right ftface) True -- keeps the ft font alive.

-- | Create a harfbuzz font that references our freetype face.
--
-- This will keep the freetype face alive until the harfbuzz object is destroyed.
hb_ft_font_create :: MonadIO m => FT.Face -> m (HB.Font RealWorld)
hb_ft_font_create ftface = liftIO do
  font <- [U.exp|hb_font_t * { hb_ft_font_create($fptr-ptr:(FT_Face ftface), NULL) }|] >>= HB.foreignFont
  font <$ HB.object_set_user_data font font_face_key ftface True -- keeps the ft font alive

-- | Create a harfbuzz face that references our freetype face. Repeated calls
-- will retrieve the same harfbuzz face. Reference management should be automatic.
hb_ft_face_create_cached :: MonadIO m => FT.Face -> m (HB.Face RealWorld)
hb_ft_face_create_cached ftface@(ForeignPtr _ guts) = liftIO do
  Ptr addr <- [U.block|hb_face_t * {
    hb_face_t * face = hb_ft_face_create_cached($fptr-ptr:(FT_Face ftface));
    hb_face_destroy(face); /* avoid off-by-1 reference counting bug */
    return face;
  }|]
  let face = HB.Face (ForeignPtr addr guts)
  face <$ HB.object_set_user_data face face_face_key (Left (unsafeForeignPtrToPtr ftface)) True -- don't try to keep the ft font alive, we are it

childPtr :: ForeignPtr a -> Ptr b -> ForeignPtr b
childPtr (ForeignPtr _ guts) (Ptr addr) = ForeignPtr addr guts

-- | Uses our own 'user-data' stash of the face rather than the hb_ft_font_get_face guts
hb_ft_font_get_face :: MonadIO m => HB.Font RealWorld -> m (Maybe FT.Face)
hb_ft_font_get_face font = liftIO $
  HB.object_get_user_data font font_face_key  >>= \case
    r@Just{} -> pure r
    Nothing -> do
      hbface@(HB.Face fp) <- get (HB.font_face font) -- relies on this getting the appropriate foreign ptr, see font_face code
      HB.object_get_user_data hbface face_face_key <&> fmap (either (childPtr fp) id)

hb_ft_font_load_flags :: HB.Font RealWorld -> StateVar RealWorld Int
hb_ft_font_load_flags font = StateVar g s where
  g = fromIntegral <$> unsafeIOToST [U.exp|int { hb_ft_font_get_load_flags($fptr-ptr:(hb_font_t * font)) }|]
  s (fromIntegral -> v) = unsafeIOToST [U.block|void { hb_ft_font_set_load_flags($fptr-ptr:(hb_font_t * font),$(int v)); }|]

-- notify harfbuzz that our freetype font has been changed. call immediately after changes are made.
hb_ft_font_changed :: MonadIO m => HB.Font RealWorld -> m ()
hb_ft_font_changed font = liftIO [U.block|void { hb_ft_font_changed($fptr-ptr:(hb_font_t * font)); }|]

hb_ft_font_set_funcs :: MonadIO m => HB.Font RealWorld -> m ()
hb_ft_font_set_funcs font = liftIO [U.block|void { hb_ft_font_set_funcs($fptr-ptr:(hb_font_t * font)); }|]
