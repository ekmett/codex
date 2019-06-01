{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module Main
( main
) where

import GHC.Exts
import Data.Text.ICU.Bidi
import System.Mem
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

spec :: Spec
spec = Hspec.after_ performMajorGC $ do
  describe "bidi" $ do
    it "can handle English" $ do
      x <- open
      getBaseDirection "hello" `shouldBe` LTR
      setPara x "hello" DEFAULT_LTR Nothing
      getLength x `shouldReturn` 5
      getText x `shouldReturn` "hello"
      writeReordered x DO_MIRRORING `shouldReturn` "hello"
      getLogicalMap x `shouldReturn` [0..4]
      getVisualMap x `shouldReturn` [0..4]
      getLevels x `shouldReturn` [Level 0,Level 0,Level 0,Level 0,Level 0]
    it "can handle Arabic" $ do
      x <- open
      getBaseDirection "أنا قادر على أكل الزجاج و هذا لا يؤلمني." `shouldBe` RTL
      setPara x "This is an Arabic sentence: أنا قادر على أكل الزجاج و هذا لا يؤلمني. It is surrounded by English" DEFAULT_LTR Nothing
      getLength x `shouldReturn` 96
      getLogicalMap x `shouldReturn` fromList ([0..27] ++ [66,65..28] ++ [67..95])
      getVisualMap x `shouldReturn` fromList ([0..27] ++ [66,65..28] ++ [67..95])
      getLevels x `shouldReturn` fromList (replicate 28 (Level 0) ++ replicate 39 (Level 1) ++ replicate 29 (Level 0))

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
