{-# options_ghc -Wno-orphans #-}
module Testing.Util
( shouldbe
, shouldreturn
, givesNEList
, resultMatches
, patternAddPropTripping
, returnsTrue
, returnsFalse
) where

import Control.Monad
import Test.Hspec
import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck.Gen as Gen

import Graphics.Fontconfig
import Graphics.Fontconfig.Internal (Result (..), FcBool (..))

givesNEList :: Show b => (a -> IO [b]) -> a -> Expectation
givesNEList f = f >=> flip shouldNotSatisfy null

shouldbe :: (Eq a, Show a) => a -> a -> Expectation
shouldbe = flip shouldBe

shouldreturn :: (Eq a, Show a) => a -> IO a -> Expectation
shouldreturn = flip shouldReturn

returnsTrue :: IO Bool -> Expectation
returnsTrue = shouldreturn True

returnsFalse :: IO Bool -> Expectation
returnsFalse = shouldreturn False

resultMatchesB :: Eq a => a -> Result a -> Bool
resultMatchesB b (ResultMatch a) = a == b
resultMatchesB _ _               = False

resultMatches :: (Show a, Eq a) => Result a -> a -> Expectation
resultMatches (ResultMatch a) b = a `shouldBe` b
resultMatches r               b = expectationFailure ("Result did not match exactly:" <> show r <> " /= " <> show b)

-- orphan instance, oh no.
instance Arbitrary FcBool where arbitrary = elements [FcTrue, FcFalse]

patternAddPropTripping
  :: ( Eq a
     , Show a
     , Arbitrary a
     )
  => [String]
  -> (Pattern -> String -> a -> IO Bool)
  -> (Pattern -> String -> Int -> IO (Result a))
  -> Maybe (a -> Bool)
  -> Property
patternAddPropTripping ps addf getf implicationf = property $ \v -> maybe True ($ v) implicationf ==>
  forAll (Gen.elements ps) (ioProperty . test v)
  where
    test v p = do
      pat <- patternCreate
      addOk <- addf pat p v
      if addOk then resultMatchesB v <$> getf pat p 0 else pure False
