{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}
module Main
( main
) where

import Control.Monad
import Data.Functor
import Data.Version
import qualified Data.List.NonEmpty as NE
import System.FilePath ((</>))
import Prelude hiding (init)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import Text.Printf
import Test.Hspec
import Test.QuickCheck hiding (Result)

import Graphics.Fontconfig
import Testing.Util

import Test.Tasty
import Test.Tasty.Hspec

testDir,confDir,fontDir :: FilePath
testDir = "test"
confDir = "test-configs"
fontDir = "test-fonts"

times12, monoMatrix100101, courier121314Styled, courierMono121314Styled, courierMonoStyled :: String
times12                 = "Times-12"
monoMatrix100101        = "Monospace:matrix=1 0.1 0 1"
courierMono121314Styled = "Courier,Monospace-12,13,14:slant=100:weight=200"
courierMonoStyled       = "Courier,Monospace:slant=100:weight=200"
courier121314Styled     = "Courier-12,13,14:slant=100:weight=200"

{-
testConfigFile :: FilePath -> String
testConfigFile fp = unlines
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE fontconfig SYSTEM \"fonts.dtd\">"
  , "<fontconfig>"
  , "  <dir>" <> fp <> "</dir>"
  , "  <description>Load empty file</description>"
  , "  <!-- Load local system customization file -->"
  , "  <include ignore_missing=\"no\">10-empty.conf</include>"
  , "</fontconfig>"
  ]
-}

main :: IO ()
main = do
  fulltd <- getCurrentDirectory <&> (</> testDir)
  userHomeDir <- getHomeDirectory

  let confD = fulltd </> confDir
      fontD = fulltd </> fontDir

      complain = True

  -- Be chatty and declare what version of the library we're testing against.
  getVersion >>= printf ("Testing against Fontconfig version: %s") . showVersion

  (testSpec "spec" >=> defaultMain) $ do
    describe "Config Loading" $ do
      it "should init" $ init `shouldReturn` True
      it "should init and bring up to date" $ initBringUptoDate `shouldReturn` True
      -- it "should init and load" $ do
      --   _ <- initLoadConfig -- Checking we can load without blowing up
      --   shouldBe True True
      -- it "should init and load with fonts" $ do
      --   _ <- initLoadConfigAndFonts
      --   shouldBe True True
      it "tolerate being reinitialised" $ do
        initReinitialize `shouldReturn` True
      it "tolerate being finalised then reinitialised" $ do
        fini
        initReinitialize `shouldReturn` True
      -- it "should die if used after finalisation" $ do
      --   c <- initLoadConfigAndFonts
      --   fini
      --   shouldThrow (configGetConfigDirs c) anyException -- Can't catch a SIGABRT !
      --   returnsTrue initReinitialize
      --   c0 <- initLoadConfigAndFonts
      --   shouldThrow (configGetConfigDirs c0) anyException -- Won't get to here :(

    describe "Config" . before initLoadConfig $ do
      it "check if up to date" $ \c -> configUptoDate c `shouldReturn` True
      describe "lists font and config directories" $ do
        it "list existing font dirs" $ givesNEList configGetFontDirs
        it "list existing config dirs" $ givesNEList configGetConfigDirs
        it "list existing config files" $ givesNEList configGetConfigFiles
        it "list existing config files" $ givesNEList configGetCacheDirs
      describe "adding font info" $ do
        it "can add a font file" $ \c -> configAppFontAddFile c (fontD </> "unifont_csur.ttf") `shouldReturn` True
        it "can add a font directory" $ \c -> configAppFontAddDir c fontD `shouldReturn` True
      describe "load and parse config files" $ do
        it "loads eevee config" $ \c ->
          configParseAndLoad c (confD </> "eevee-font-conf.conf") complain `shouldReturn` True

    describe "Range" $ do
      it "tripping integer range" $ property $ \l u -> do
        range <- rangeCreateInteger l u
        rangeGetDouble range `shouldReturn` Just (fromIntegral l, fromIntegral u)

      it "tripping double range" $ property $ \l u -> do
        range <- rangeCreateDouble l u
        rangeGetDouble range `shouldReturn` Just (l, u)

      describe "copying range" $ before (rangeCreateInteger 0 10 >>= \r -> (r,) <$> rangeCopy r) $ do
        it "won't have matching pointers" $ \(og, copy) ->
          og `shouldNotBe` copy
        it "will have matching values" $ \(og, copy) -> do
          r1 <- rangeGetDouble og
          r2 <- rangeGetDouble copy
          r1 `shouldBe` r2

    describe "Pattern" $ do
      it "empty pattern is empty string" $
        patternCreate >>= nameUnparse >>= flip shouldSatisfy null

      it "parse trivial patterns" $ do
        let checkPattern s = nameParse s >>= nameUnparse >>= flip shouldBe s
        checkPattern times12
        checkPattern monoMatrix100101
        checkPattern "Times-12:weight=200" -- bold
        checkPattern "Courier:slant=100"   -- italic

      it "formats the pattern" $ do
        p <- nameParse "Courier-12:slant=100"
        patternFormat p "Family: %{family}, Size: %{size}, Style: %{slant=}"
          `shouldReturn` Just "Family: Courier, Size: 12, Style: slant=100"

      describe "matches patterns" $ do
        it "can match patterns" $ do
          pA <- nameParse times12
          pB <- nameParse times12
          patternEqual pA pB `shouldReturn` True

        it "won't match non-similar patterns" $ do
          pA <- nameParse times12
          pB <- nameParse "Times-12:slant=100"
          patternEqual pA pB `shouldReturn` False

      describe "duplicates patterns" $ before (nameParse times12 >>= \pA -> (pA,) <$> patternDuplicate pA) $ do
        it "will have different pointers" $ \(og, copy) -> do
          og `shouldNotBe` copy

        it "will be the same pattern" $ \(og, copy) -> do
          patternEqual og copy `shouldReturn` True

      it "matches patterns given object set" $ do
        pA <- nameParse courierMono121314Styled
        pB <- nameParse "Courier-12,13,14"
        (objectSet (NE.fromList ["size"]) >>= patternEqualSubset pA pB) `shouldReturn` True
        (objectSet (NE.fromList ["family"]) >>= patternEqualSubset pA pB) `shouldReturn` False

      it "filters patterns using object set" $ do
        pA <- nameParse courierMono121314Styled
        let pAFiltered = "Courier,Monospace-12,13,14"
        os <- objectSet (NE.fromList ["family","size"])
        (patternFilter pA (Just os) >>= nameUnparse) `shouldReturn` pAFiltered

      it "filter patterns with empty object set duplicate pattern" $ do
        let pat = "Courier,Monospace-12,13,14:slant=100:weight=200"
        pA <- nameParse pat
        (patternFilter pA Nothing >>= nameUnparse) `shouldReturn` pat

      describe "Adding values to patterns" $ do
        it "adds integer to the pattern value listed in objectset" $ patternAddPropTripping
          ["slant", "weight", "width", "spacing", "hintstyle", "index", "rgba", "lcdfilter", "fontversion"]
          patternAddInteger
          patternGetInteger
          Nothing

        it "adds double to the pattern value listed in objectset" $ patternAddPropTripping
          ["size", "aspect", "pixelsize", "scale", "dpi"]
          patternAddDouble
          patternGetDouble
          Nothing

        it "adds bool to the pattern value listed in objectset" $ patternAddPropTripping
          ["antialias", "hinting", "verticallayout", "autohint", "globaladvance", "outline", "scalable", "color", "minspace", "embolden", "embeddedbitmap", "decorative"]
          patternAddBool
          patternGetBool
          Nothing

        it "adds string to the pattern value listed in objectset" $ patternAddPropTripping
          ["family" ,"familylang" ,"style" ,"stylelang" ,"fullname" ,"fullnamelang" ,"foundry" ,"file" ,"rasterizer" ,"lang" ,"capability" ,"fontformat" ,"fontfeatures" ,"namelang" ,"prgname" ,"postscriptname"]
          patternAddString
          patternGetString
          $ pure (not . any (== '\NUL'))

      describe "remove/delete values from patterns correctly" . before (nameParse courierMono121314Styled) $ do
        it "patternDel: all values for existing property are are removed" $ \p -> do
          patternDel p "size" `shouldReturn` True
          nameUnparse p `shouldReturn` courierMonoStyled
        it "patternDel: missing property returns false" $ \p ->
          patternDel p "foundry" `shouldReturn` False

        it "patternRemove: existing value on object is removed" $ \p -> do
          patternRemove p "family" 1 `shouldReturn` True
          nameUnparse p `shouldReturn` courier121314Styled
        it "patternRemove: missing 'ith value for existing property returns false" $ \p ->
          patternRemove p "family" 3 `shouldReturn` False
        it "patternRemove: missing value on object returns false" $ \p ->
          patternRemove p "postscriptname" 0 `shouldReturn` False

--- failing tests with explanations below

      it "can read home directory" $ do
        homeOn <- configEnableHome True
        when homeOn $ configHome `shouldReturn` Just userHomeDir
