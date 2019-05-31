module Main where

import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import System.Mem (performMajorGC)
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Graphics.FreeType.Internal
import Graphics.FreeType.Library

spec :: Spec
spec = Hspec.after_ performMajorGC $ do
  describe "Library" $ do
    it "can be constructed from scratch" $ do
      Library lib <- new_library
      unsafeForeignPtrToPtr lib `shouldNotBe`  nullPtr
    it "can be constructed with defaults" $ do
      Library lib <- init_library
      unsafeForeignPtrToPtr lib `shouldNotBe`  nullPtr

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
