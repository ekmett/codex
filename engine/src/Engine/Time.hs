{-# language TypeApplications #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Engine.Time
  ( now
  , Time
  , addTime
  , diffTime
  , resolution
  , approximateUTCTime
  ) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Functor ((<&>))
import Data.Time.Clock
import Data.Word
import SDL.Raw.Timer
import System.IO.Unsafe

-- | High resolution performance timer.
--
-- Provides a simple monotonic system clock in seconds with undefined origin
newtype Time = Time { getTime :: Word64 } deriving (Eq,Ord,Show)

now :: MonadIO m => m Time
now = liftIO $ coerce (getPerformanceCounter @IO)
{-# inline now #-}

addTime :: NominalDiffTime -> Time -> Time
addTime dt (Time t) = Time $ round (realToFrac dt * resolution') + t
{-# inline addTime #-}

diffTime :: Time -> Time -> NominalDiffTime
diffTime (Time t) (Time s)
  | t - s < t = realToFrac $ fromIntegral (t - s) / frequency
  | otherwise = realToFrac $ negate $ fromIntegral (s - t) / frequency
{-# inline diffTime #-}

resolution :: NominalDiffTime
resolution = realToFrac resolution'
{-# inline resolution #-}

resolution' :: Double
resolution' = recip frequency
{-# inline resolution' #-}

frequency :: Double
frequency = fromIntegral $ unsafeDupablePerformIO getPerformanceFrequency
{-# noinline frequency #-}

-- | Total hack to roughly extract at time stamp. Don't do this.
approximateUTCTime :: Time -> IO UTCTime
approximateUTCTime t = do
  actual <- getCurrentTime
  now <&> \s -> addUTCTime (diffTime s t) actual
{-# inlinable approximateUTCTime #-}
