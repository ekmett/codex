{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module Main
( main
) where

-- import Data.Default
import Data.Text.Bidirectional
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

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
