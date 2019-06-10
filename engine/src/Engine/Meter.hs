{-# language MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Engine.Meter
  ( Meter
  , tick
  , fps
  ) where

import Data.Data
import Data.Default
import Data.FingerTree
import Engine.Time

data Ticks
  = NoTicks
  | Ticks {-# UNPACK #-} !Time {-# UNPACK #-} !Time {-# UNPACK #-} !Int
  deriving Show

instance Semigroup Ticks where
  NoTicks     <> m            = m
  m           <> NoTicks      = m
  Ticks l _ n <> Ticks _ h n' = Ticks l h (n + n')

instance Monoid Ticks where
  mempty = NoTicks

newtype Tick = Tick Time deriving Show

instance Measured Ticks Tick where
  measure (Tick d) = Ticks d d 1

newtype Meter = Meter { _meterTicks :: FingerTree Ticks Tick }
  deriving (Show, Typeable)

instance Default Meter where
  def = Meter mempty

instance Measured Ticks Meter where
  measure (Meter t) = measure t

-- | record a tick at a given time
tick :: Time -> Meter -> Meter
tick d (Meter t) = Meter $ dropUntil newEnough (t |> Tick d) where
  newEnough NoTicks       = False
  newEnough (Ticks _ h _) = h >= addTime (-5) d

-- | returns the current number of ticks per second over the last few seconds.
fps :: Meter -> Double
fps (Meter t) = case measure t of
  Ticks l h n | h > l -> (fromIntegral n - 1) / realToFrac (diffTime h l)
  _ -> 0
