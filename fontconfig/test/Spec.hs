{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (init)

import Debug.Trace (traceShowM,traceShowId)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import Data.Functor
import Data.Version
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck.Gen as Gen

import System.Directory (getCurrentDirectory, getHomeDirectory, doesDirectoryExist, doesPathExist)
import Text.Printf

import Graphics.Fontconfig
import Graphics.Fontconfig.Internal (Result (..), FcBool (..))

import Testing.Util

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

main :: IO ()
main = do
  fulltd <- getCurrentDirectory <&> (</> testDir)
  userHomeDir <- getHomeDirectory

  let confD = fulltd </> confDir
      fontD = fulltd </> fontDir

      complain = True

  -- Be chatty and declare what version of the library we're testing against.
  getVersion >>= printf ("Testing against Fontconfig version: %s") . showVersion

  hspec $ do
    describe "Config Loading" $ do
      it "should init" $ returnsTrue init
      it "should init and bring up to date" $ returnsTrue initBringUptoDate
      -- it "should init and load" $ do
      --   _ <- initLoadConfig -- Checking we can load without blowing up
      --   shouldBe True True
      -- it "should init and load with fonts" $ do
      --   _ <- initLoadConfigAndFonts
      --   shouldBe True True
      it "tolerate being reinitialised" $ do
        returnsTrue initReinitialize
      it "tolerate being finalised then reinitialised" $ do
        fini
        returnsTrue initReinitialize
      -- it "should die if used after finalisation" $ do
      --   c <- initLoadConfigAndFonts
      --   fini 
      --   shouldThrow (configGetConfigDirs c) anyException -- Can't catch a SIGABRT !
      --   returnsTrue initReinitialize
      --   c0 <- initLoadConfigAndFonts
      --   shouldThrow (configGetConfigDirs c0) anyException -- Won't get to here :(

    describe "Config" . before initLoadConfig $ do
      it "check if up to date" $ \c -> returnsTrue (configUptoDate c)
      describe "lists font and config directories" $ do
        it "list existing font dirs" $ givesNEList configGetFontDirs
        it "list existing config dirs" $ givesNEList configGetConfigDirs
        it "list existing config files" $ givesNEList configGetConfigFiles
        it "list existing config files" $ givesNEList configGetCacheDirs
      describe "adding font info" $ do
        it "can add a font file" $ \c -> returnsTrue $ configAppFontAddFile c (fontD </> "unifont_csur.ttf")
        it "can add a font directory" $ \c -> returnsTrue $ configAppFontAddDir c fontD
      describe "load and parse config files" $ do
        it "loads eevee config" $ \c -> returnsTrue $
          configParseAndLoad c (confD </> "eevee-font-conf.conf") complain

    describe "Range" $ do
      let compareRange l u = maybe False (== (l,u))

      it "tripping integer range" $ property $ \l u -> ioProperty $
          rangeCreateInteger l u >>= fmap ( compareRange (fromIntegral l) (fromIntegral u) ) . rangeGetDouble

      it "tripping double range" $  property $ \l u -> ioProperty $
          rangeCreateDouble l u >>= fmap ( compareRange l u ) . rangeGetDouble

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
        nameParse "Courier-12:slant=100"
          >>= flip patternFormat "Family: %{family}, Size: %{size}, Style: %{slant=}"
          >>= flip shouldBe (Just "Family: Courier, Size: 12, Style: slant=100")

      describe "matches patterns" $ do
        it "can match patterns" $ do
          pA <- nameParse times12
          pB <- nameParse times12
          returnsTrue $ patternEqual pA pB

        it "won't match non-similar patterns" $ do
          pA <- nameParse times12
          pB <- nameParse "Times-12:slant=100"
          returnsFalse $ patternEqual pA pB

      describe "duplicates patterns" $ before (nameParse times12 >>= \pA -> (pA,) <$> patternDuplicate pA) $ do
        it "will have different pointers" $ \(og, copy) -> do
          og `shouldNotBe` copy
        it "will be the same pattern" $ \(og, copy) -> do
          returnsTrue $ patternEqual og copy

      it "matches patterns given object set" $ do
        pA <- nameParse courierMono121314Styled
        pB <- nameParse "Courier-12,13,14"
        returnsTrue $ objectSet (NE.fromList ["size"]) >>= patternEqualSubset pA pB
        returnsFalse $ objectSet (NE.fromList ["family"]) >>= patternEqualSubset pA pB

      it "filters patterns using object set" $ do
        pA <- nameParse courierMono121314Styled
        let pAFiltered = "Courier,Monospace-12,13,14"
        shouldbeM pAFiltered $ objectSet (NE.fromList ["family","size"]) >>= patternFilter pA >>= nameUnparse

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
          returnsTrue $ patternDel p "size"
          shouldbeM courierMonoStyled $ nameUnparse p
        it "patternDel: missing property returns false" $ \p ->
          returnsFalse $ patternDel p "foundry"

        it "patternRemove: existing value on object is removed" $ \p -> do
          returnsTrue $ patternRemove p "family" 1
          shouldbeM courier121314Styled $ nameUnparse p
        it "patternRemove: missing 'ith value for existing property returns false" $ \p ->
          returnsFalse $ patternRemove p "family" 3
        it "patternRemove: missing value on object returns false" $ \p ->
          returnsFalse $ patternRemove p "postscriptname" 0

--- failing tests with explanations below
  
      -- We can either support the behaviour of duplicating the
      -- pattern when given a NULL object set, or prevent people from
      -- creating NULL object sets. As an object set created from `[]`
      -- is not NULL and does not behave the same way.
      --
      -- it "filter patterns with empty object set duplicate pattern" $ do
      --   let pat = "Courier,Monospace-12,13,14:slant=100:weight=200"
      --   pA <- nameParse pat
      --   shouldbeM pat $ objectSet [] >>= patternFilter pA >>= nameUnparse


      -- Fails with:
      --
      -- munmap_chunk(): invalid pointer
      -- fish: “cabal new-run pkg:fontconfig:te…” terminated by signal SIGABRT (Abort)
      --
      -- it "can read home directory" $ do
      --   homeOn <- configEnableHome True
      --   when homeOn $ configHome >>= (`shouldBe` userHomeDir)
