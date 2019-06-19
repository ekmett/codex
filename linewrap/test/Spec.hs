{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Hedgehog (Range, Gen, Property, forAll, property, (===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Text (Text)
import qualified Data.Text as T

import Text.Hyphenation (english_GB)

import Data.LineWrap.Types
import Data.LineWrap.BruteForce
import Data.LineWrap.DynamicProg
import Data.LineWrap.ShortestPath
import Data.LineWrap.BinarySearch
import Data.LineWrap.Linear

tinycaseRaw :: Text
tinycaseRaw = "really beautiful that the sun"

-- Wrapped with: Ideal 10, Max 12
tinycaseWrapped :: [Text]
tinycaseWrapped = ["really", "beautiful", "that the sun"]

midtestcase :: Text
midtestcase = "The quick brown fox jumps over the lazy brown dog a few times, and again for good measure."

bigtestcaseRaw :: Text
bigtestcaseRaw = "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face"

bigtestcaseWrappedAt64 :: [Text]
bigtestcaseWrappedAt64 =
  [ "In olden times when wishing still helped one, there lived a king"
  , "whose daughters were all beautiful, but the youngest was so beau-"
  , "tiful that the sun itself, which has seen so much, was astonished"
  , "whenever it shone in her face"
  ]

-- Line length 64
--
-- High badness:
-- In  olden  times  when  wishing  still  helped  one,  there
-- lived a king whose daughters were all beautiful, but the youngest
-- was  so  beautiful  that  the  sun  itself,  which  has  seen  so
-- much, was astonished whenever it shone in her face
--
-- Low badness:
-- In olden times when wishing still helped one, there lived a king
-- whose daughters were all beautiful, but the youngest was so beau-
-- tiful that the sun itself, which has seen so much, was astonished
-- whenever it shone in her face

genWords :: Range Int -> Gen Text
genWords n = T.unwords <$> Gen.list n (Gen.text (Range.linear 1 20) Gen.alphaNum)

prop_linebreaking :: Range Int -> (Int -> Range Int) -> (Text -> Width -> [Text]) -> Property
prop_linebreaking widthRange wordsRange breaker = property $ do
  width <- forAll $ Gen.int widthRange -- (Range.linear 0 100)
  input <- forAll $ genWords (wordsRange width) -- (Range.linear 0 (width * 10))
  let
    broken = breaker input (Width width)

  if T.null input
    then do
        HH.annotate "Empty input remains empty"
        broken === [""]

    else do
      -- maximum is partial, joy of joys
      let maxWordSize = maximum $ T.length <$> T.words input

      -- if we have words that exceed the desired width, or we gen a
      -- width of 1 then this property doesn't make much sense.
      when (maxWordSize <= width) $ do
        HH.annotate "All lines are under or equal to desired width"
        HH.annotateShow broken
        HH.assert $ all ((<= width) . T.length) broken

      HH.annotate "No input is lost"
      T.unwords broken === input

main :: IO ()
main = defaultMain $ testGroup "Line Wrap (no hyphenation)"
  [ testGroup "Tiny Input"
    [ testCase "bruteForce" $ bruteForce tinycaseRaw (Width 12) @?= tinycaseWrapped
    , testCase "dynamicProgramming" $
      wText dynamicProgramming tinycaseRaw (Width 12) @?= tinycaseWrapped
    , testCase "shortestPath" $
      wText shortestPath tinycaseRaw (Width 12) @?= tinycaseWrapped
    , testCase "linear" $
      wText linear tinycaseRaw (Width 12) @?= tinycaseWrapped

    ]

  , testGroup "Properties"
    [
    --  testProperty "bruteForce (limited to: Width 0 - 15, Words 0 - 20)" $
    --    prop_linebreaking (Range.linear 0 15) (const $ Range.linear 0 20) bruteForce

      propWText "dynamicProgramming" dynamicProgramming
    , propWText "shortestPath" shortestPath
    , propWText "binarySearch" binarySearch
    , propWText "linear" linear
    ]
  ]
  where
    wText f = f T.unwords T.words T.length
    propWText n = testProperty n
      . prop_linebreaking (Range.linear 0 100) (\w -> Range.linear 0 (w * 10))
      . wText
