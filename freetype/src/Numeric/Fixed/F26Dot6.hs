{-# LANGUAGE CApiFFI, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- | Signed 25.6 2s complement fixed precision.
--
-- Sometimes called "26.6" in literature on typefaces.
module Numeric.Fixed.F26Dot6 
  ( F26Dot6(..)
  , fromF26Dot6
  , toF26Dot6
  ) where

import Data.Bits
import Data.Coerce
import Data.Int
import Data.Ratio
import Data.Typeable
import Foreign.Storable
import Foreign.C.Types

-- | A signed 2s complement 25.6 scale fixed precision number
newtype {-# CTYPE "signed int" #-} F26Dot6 = F26Dot6 { getF26Dot6 :: CInt } deriving (Eq,Ord,Typeable,Storable)

fromF26Dot6 :: F26Dot6 -> Double
fromF26Dot6 (F26Dot6 x) = fromIntegral x / 64

toF26Dot6 :: Double -> F26Dot6
toF26Dot6 x = F26Dot6 $ floor (x * 64 + 0.5)

instance Show F26Dot6 where
  showsPrec d = showsPrec d . fromF26Dot6

instance Num F26Dot6 where
  (+) = coerce ((+) :: CInt -> CInt -> CInt)
  (-) = coerce ((-) :: CInt -> CInt -> CInt)
  negate = coerce (negate :: CInt -> CInt)
  abs = coerce (abs :: CInt -> CInt)
  signum (F26Dot6 a) = F26Dot6 $ unsafeShiftL (signum a) 6
  F26Dot6 a * F26Dot6 b = F26Dot6 $ fromIntegral (unsafeShiftR (fromIntegral a * fromIntegral b) 6 :: Int64)
  fromInteger i = F26Dot6 $ unsafeShiftL (fromInteger i) 6

instance Enum F26Dot6 where
  succ (F26Dot6 a) = F26Dot6 (a + 64)
  pred (F26Dot6 a) = F26Dot6 (a - 64)
  fromEnum = truncate
  toEnum a = F26Dot6 (unsafeShiftL (fromIntegral a) 6)
  enumFrom a           = toF26Dot6 `map` enumFrom (fromF26Dot6 a)
  enumFromTo a b       = toF26Dot6 `map` enumFromTo (fromF26Dot6 a) (fromF26Dot6 b)
  enumFromThen a b     = toF26Dot6 `map` enumFromThen (fromF26Dot6 a) (fromF26Dot6 b)
  enumFromThenTo a b c = toF26Dot6 `map` enumFromThenTo (fromF26Dot6 a) (fromF26Dot6 b) (fromF26Dot6 c)

instance Bounded F26Dot6 where
  minBound = F26Dot6 minBound
  maxBound = F26Dot6 maxBound

instance Fractional F26Dot6 where
  F26Dot6 a / F26Dot6 b  = F26Dot6 $ fromIntegral (unsafeShiftL (fromIntegral a) 6 `div` fromIntegral b :: Int64)
  fromRational a = F26Dot6 $ fromInteger (unsafeShiftL (numerator a) 6 `div` denominator a)

instance Real F26Dot6 where
  toRational (F26Dot6 i) = toInteger i % 64

instance RealFrac F26Dot6 where
  properFraction (F26Dot6 a) 
    | a >= 0 = (fromIntegral (unsafeShiftR a 6), F26Dot6 (a .&. 63))
    | otherwise = (negate $ fromIntegral $ unsafeShiftR (negate a) 6, F26Dot6 $ (a .&. 63) - 64)
  truncate (F26Dot6 a) 
    | a >= 0 = fromIntegral (unsafeShiftR a 6)
    | otherwise = negate $ fromIntegral $ unsafeShiftR (negate a) 6
  round (F26Dot6 f)   = fromIntegral $ unsafeShiftR (f + 32) 6 
  ceiling (F26Dot6 f) = fromIntegral $ unsafeShiftR (f + 63) 6
  floor (F26Dot6 f)   = fromIntegral $ unsafeShiftR f 6

instance Floating F26Dot6 where
  pi = toF26Dot6 pi
  exp = toF26Dot6 . exp . fromF26Dot6
  sqrt = toF26Dot6 . sqrt . fromF26Dot6
  log = toF26Dot6 . log . fromF26Dot6
  a ** b = toF26Dot6 $ fromF26Dot6 a ** fromF26Dot6 b
  logBase a b = toF26Dot6 $ logBase (fromF26Dot6 a) (fromF26Dot6 b)
  sin = toF26Dot6 . sin . fromF26Dot6
  tan = toF26Dot6 . tan . fromF26Dot6
  cos = toF26Dot6 . cos . fromF26Dot6
  asin = toF26Dot6 . asin . fromF26Dot6
  atan = toF26Dot6 . atan . fromF26Dot6
  acos = toF26Dot6 . acos . fromF26Dot6
  sinh = toF26Dot6 . sinh . fromF26Dot6
  tanh = toF26Dot6 . tanh . fromF26Dot6
  cosh = toF26Dot6 . cosh . fromF26Dot6
  asinh = toF26Dot6 . asinh . fromF26Dot6
  atanh = toF26Dot6 . atanh . fromF26Dot6
  acosh = toF26Dot6 . acosh . fromF26Dot6

instance RealFloat F26Dot6 where
  floatRadix  _ = 2
  floatDigits _ = 6
  decodeFloat = decodeFloat . fromF26Dot6
  isInfinite _ = False
  isIEEE _ = False
  atan2 a b = toF26Dot6 $ atan2 (fromF26Dot6 a) (fromF26Dot6 b)
  isDenormalized (F26Dot6 a) = a .&. 0x7fffffc0 /= 0
  isNaN _ = False
  isNegativeZero _ = False
  floatRange _ = (5,0)
  encodeFloat i j = toF26Dot6 $ encodeFloat i j
  exponent = exponent . fromF26Dot6
  significand = toF26Dot6 . significand . fromF26Dot6
  scaleFloat n (F26Dot6 a) = F26Dot6 (shift a n)
