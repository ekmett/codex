{-# language OverloadedStrings #-}
module Main
( main
) where

import qualified Data.ByteString as ByteString
import Data.Coerce
import Data.Word
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import System.Mem (performMajorGC)
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Graphics.FreeType.Internal
import Graphics.FreeType.Library
import Graphics.FreeType.Face

c2w :: Char -> Word32
c2w = fromIntegral . fromEnum

spec :: Spec
spec = Hspec.after_ performMajorGC $ do
  describe "Library" $ do
    it "can be constructed from scratch" $ do
      lib <- new_library
      unsafeForeignPtrToPtr (coerce lib) `shouldNotBe`  nullPtr
    it "can be constructed with defaults" $ do
      lib <- init_library
      unsafeForeignPtrToPtr (coerce lib) `shouldNotBe`  nullPtr
    it "can be reference counted" $ do
      lib <- init_library
      reference_library lib
      done_library lib `shouldReturn` () -- we don't crash during the subsequent gc
  describe "Face" $ do
    it "can load a file" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0
      has_multiple_masters face `shouldReturn` False
      get_char_index face (c2w 'a') `shouldReturn` 28 -- mined from the font itself
      get_first_char face `shouldReturn` (c2w ' ',1)
      get_next_char face (c2w ' ') `shouldReturn` (c2w '!',918)
      get_font_format face `shouldReturn` "CFF"
      reference_face face
      done_face face -- validate that this doesn't crash during gc
    it "can load a variable otf font (from memory)" $ do
      lib <- init_library
      blob <- ByteString.readFile "test/fonts/SourceCodeVariable-Roman.otf"
      face <- new_memory_face lib blob 0
      has_multiple_masters face `shouldReturn` True
      get_char_index face (c2w 'a') `shouldReturn` 28 -- mined from the font itself
      get_font_format face `shouldReturn` "CFF"
      get_first_char face `shouldReturn` (c2w ' ',1)
      set_pixel_sizes face 32 32 -- we can set the current pixel size
    it "can load a variable ttf font" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodeVariable-Roman.ttf" 0
      has_multiple_masters face `shouldReturn` True
      get_char_index face (c2w 'a') `shouldReturn` 28 -- mined from the font itself
      get_first_char face `shouldReturn` (c2w ' ',1)
      get_font_format face `shouldReturn` "TrueType"

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
