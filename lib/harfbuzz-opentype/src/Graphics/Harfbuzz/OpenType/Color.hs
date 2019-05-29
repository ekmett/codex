{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
-- |
module Graphics.Harfbuzz.OpenType.Color
(
-- * Color
  Color(..)
, ColorLayer(..)
-- ** svg
, color_has_svg
, color_glyph_reference_svg
-- ** png
, color_has_png
, color_glyph_reference_png
-- ** layered glyphs
, color_has_palettes
, color_palette_get_count
, color_palette_get_name_id
, color_palette_get_flags
, color_palette_get_colors
, color_palette_color_get_name_id
, color_has_layers
, color_glyph_get_layers
) where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Functor ((<&>))
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Prim (copyAddrToByteArray#)
import GHC.Ptr (Ptr(Ptr))
import GHC.Types (Int(I#))
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- * Color

color_has_svg :: MonadIO m => Face -> m Bool
color_has_svg face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_svg($face:face) }|] <&> cbool

color_glyph_reference_svg :: MonadIO m => Face -> Codepoint -> m Blob
color_glyph_reference_svg face glyph = liftIO $ [C.exp|hb_blob_t * { hb_ot_color_glyph_reference_svg($face:face,$(hb_codepoint_t glyph)) }|] >>= foreignBlob

color_has_png :: MonadIO m => Face -> m Bool
color_has_png face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_png($face:face) }|] <&> cbool

-- | Why does this take a font and the svg option take a face?! *sigh*
color_glyph_reference_png :: MonadIO m => Font -> Codepoint -> m Blob
color_glyph_reference_png font glyph = liftIO $ [C.exp|hb_blob_t * { hb_ot_color_glyph_reference_png($font:font,$(hb_codepoint_t glyph)) }|] >>= foreignBlob

color_has_palettes :: MonadIO m => Face -> m Bool
color_has_palettes face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_palettes($face:face) }|] <&> cbool

color_palette_get_count :: MonadIO m => Face -> m Int
color_palette_get_count face = liftIO $ [C.exp|unsigned int { hb_ot_color_palette_get_count($face:face) }|] <&> fromIntegral

color_palette_get_name_id :: MonadIO m => Face -> Int -> m Name
color_palette_get_name_id face (fromIntegral -> palette_index) = liftIO
  [C.exp|hb_ot_name_id_t { hb_ot_color_palette_get_name_id($face:face,$(unsigned int palette_index)) }|]

color_palette_color_get_name_id :: MonadIO m => Face -> Int -> m Name
color_palette_color_get_name_id face (fromIntegral -> color_index) = liftIO
  [C.exp|hb_ot_name_id_t { hb_ot_color_palette_color_get_name_id($face:face,$(unsigned int color_index)) }|]

color_palette_get_flags :: MonadIO m => Face -> Int -> m ColorPaletteFlags
color_palette_get_flags face (fromIntegral -> palette_index) = liftIO
  [C.exp|hb_ot_color_palette_flags_t { hb_ot_color_palette_get_flags($face:face,$(unsigned int palette_index)) }|]

-- still missing from Data.Primitive.PrimArray as of 0.7
copyPtrToPrimArray :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Ptr a -> Int -> m ()
copyPtrToPrimArray (MutablePrimArray mba) ((I# (sizeOf# @a undefined) *) -> I# offset) (Ptr addr) ((I# (sizeOf# @a undefined) *) -> I# len) =
  primitive_ $ \s -> copyAddrToByteArray# addr mba offset len s

color_palette_get_colors :: MonadIO m => Face -> Int -> m (PrimArray Color)
color_palette_get_colors face (fromIntegral -> palette_index) = liftIO $
  withSelf face $ \f -> do
    n@(fromIntegral -> i) <- [C.exp|unsigned int { hb_ot_color_palette_get_colors($(hb_face_t * f),$(unsigned int palette_index),0,0,0) }|]
    allocaArray i $ \pcolors ->
      with n $ \pn -> do
        _ <- [C.exp|unsigned int { hb_ot_color_palette_get_colors($(hb_face_t * f),$(unsigned int palette_index),0,$(unsigned int * pn),$(hb_color_t * pcolors)) }|]
        mpa <- newPrimArray i
        copyPtrToPrimArray mpa 0 pcolors i
        unsafeFreezePrimArray mpa

color_has_layers :: MonadIO m => Face -> m Bool
color_has_layers face = liftIO $ [C.exp|hb_bool_t { hb_ot_color_has_layers($face:face) }|] <&> cbool

color_glyph_get_layers_ :: Face -> Codepoint -> Int -> Int -> IO (Int,Int,[ColorLayer])
color_glyph_get_layers_ face glyph (fromIntegral -> start_offset) requested_layers_count = liftIO $
    with (fromIntegral requested_layers_count) $ \players_count ->
      allocaArray requested_layers_count $ \players -> do
        total_number_of_layers <- [C.exp|unsigned int {
          hb_ot_color_glyph_get_layers(
            $face:face,
            $(hb_codepoint_t glyph),
            $(unsigned int start_offset),
            $(unsigned int * players_count),
            $(hb_ot_color_layer_t * players)
          )
        }|] <&> fromIntegral
        retrieved_layers_count <- fromIntegral <$> peek players_count
        layers <- peekArray retrieved_layers_count players
        pure (total_number_of_layers, retrieved_layers_count, layers)

color_glyph_get_layers :: MonadIO m => Face -> Codepoint -> m [ColorLayer]
color_glyph_get_layers face glyph = liftIO $ do
  (total, retrieved, layers) <- color_glyph_get_layers_ face glyph 0 8
  if total == retrieved
  then return layers
  else color_glyph_get_layers_ face glyph 8 (total - 8) <&> \(_,_,layers2) -> layers ++ layers2
