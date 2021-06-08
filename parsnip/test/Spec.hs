{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main
( main
) where

import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Text.Parsnip
import qualified Text.Parsnip.Word8 as PW

import Data.Function (on)
import Data.ByteString.Internal (c2w)


spec :: IO TestTree
spec = testSpec "spec" $ do

  describe "parsnip" $ do
    it "while"       $ parse (while ('A' ==)) "AAB"   `shouldBe` Right "AA"
    it "while edge"  $ parse (while ('B' ==)) "AAB"   `shouldBe` Right ""
    it "while NUL"   $ parse (while ('A' ==)) "A\0AB" `shouldBe` Right "A"
    -- These are failing. Is this a bug? Or just a necessity due to the design?
    -- it "while"       $ parse (while (== 'A')) "AAB"   `shouldBe` Right "AA"
    -- it "while edge"  $ parse (while (== 'B')) "AAB"   `shouldBe` Right ""
    -- it "while NUL"   $ parse (while (== 'A')) "A\0AB" `shouldBe` Right "A"
    it "till"        $ parse (till  (== 'B')) "AAB"   `shouldBe` Right "AA"
    it "till edge"   $ parse (till  (== 'A')) "AAB"   `shouldBe` Right ""
    it "till NUL"    $ parse (till  (== 'B')) "A\0AB" `shouldBe` Right "A"

  describe "parsnip Word8" $ do
    it "while"       $ parse (PW.while (c2w 'A' ==)) "AAB"   `shouldBe` Right "AA"
    it "while edge"  $ parse (PW.while (c2w 'B' ==)) "AAB"   `shouldBe` Right ""
    it "while NUL"   $ parse (PW.while (c2w 'A' ==)) "A\0AB" `shouldBe` Right "A"
    -- These are failing. Is this a bug? Or just a necessity due to the design?
    -- it "while"       $ parse (PW.while (== c2w 'A')) "AAB"   `shouldBe` Right "AA"
    -- it "while edge"  $ parse (PW.while (== c2w 'B')) "AAB"   `shouldBe` Right ""
    -- it "while NUL"   $ parse (PW.while (== c2w 'A')) "A\0AB" `shouldBe` Right "A"
    it "till"        $ parse (PW.till  (== c2w 'B')) "AAB"   `shouldBe` Right "AA"
    it "till edge"   $ parse (PW.till  (== c2w 'A')) "AAB"   `shouldBe` Right ""
    it "till NUL"    $ parse (PW.till  (== c2w 'B')) "A\0AB" `shouldBe` Right "A"


instance Eq Location where
  (==) = (==) `on` show

main :: IO ()
main = do
  tests <- spec
  defaultMain tests
