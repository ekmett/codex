module Main
( main
) where

import Data.Coerce
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import System.Mem (performMajorGC)
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Graphics.FreeType.Internal
import Graphics.FreeType.Library
import Graphics.FreeType.Face

spec :: Spec
spec = Hspec.after_ performMajorGC $ do
  describe "Library" $ do
    it "can be constructed from scratch" $ do
      lib <- new_library
      unsafeForeignPtrToPtr (coerce lib) `shouldNotBe`  nullPtr
    it "can be constructed with defaults" $ do
      lib <- init_library
      unsafeForeignPtrToPtr (coerce lib) `shouldNotBe`  nullPtr
  describe "Face" $ do
    it "can load a file" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0
      get_char_index face 'a' `shouldReturn` 28 -- mined from the font itself
    it "can load a variable otf font" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodeVariable-Roman.otf" 0
      get_char_index face 'a' `shouldReturn` 28 -- mined from the font itself
    it "can load a variable ttf font" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodeVariable-Roman.ttf" 0
      get_char_index face 'a' `shouldReturn` 28 -- mined from the font itself

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
