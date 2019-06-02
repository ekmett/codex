module Testing.Util where

import Control.Monad
import Data.Functor
import Data.Version
import Test.Hspec
import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck.Gen as Gen

import Text.Printf

import Graphics.Fontconfig
import Graphics.Fontconfig.Internal (Result (..), FcBool (..))

infixr 9 </>

(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a <> "/" <> b

givesNEList :: Show b => (a -> IO [b]) -> a -> Expectation
givesNEList f = f >=> flip shouldNotSatisfy null

shouldbe :: (Eq a, Show a) => a -> a -> Expectation
shouldbe = flip shouldBe

shouldbeM :: (Eq a, Show a) => a -> IO a -> Expectation
shouldbeM a f = f >>= shouldbe a

returnsTrue :: IO Bool -> Expectation
returnsTrue = shouldbeM True

returnsFalse :: IO Bool -> Expectation
returnsFalse = shouldbeM False

resultMatchesB :: (Show a, Eq a) => a -> Result a -> Bool
resultMatchesB b (ResultMatch a) = a == b
resultMatchesB b r               = False

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

    checkResult v p Nothing = expectationFailure $ printf "Failed to add value of %s to property %s" (show v) p
    checkResult v _ (Just r) = resultMatches v r
