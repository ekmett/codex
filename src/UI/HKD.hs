{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
-- | "Higher-Kinded Data" such as it is
module UI.HKD 
  ( type (~>)
  , FFunctor(..)
  , FContravariant(..)
  , Log(..)
  , Tab(..)
  , indexLog
  ) where

import Data.Packing

type f ~> g = forall a. f a -> g a

class FFunctor t where
  ffmap :: (f ~> g) -> t f -> t g

class FContravariant t where
  fcontramap :: (f ~> g) -> t g -> t f

newtype Log f = Log { runLog :: forall a. f a -> a }

indexLog :: f a -> Log f -> a
indexLog fa (Log fa2a) = fa2a fa

instance FContravariant Log where
  fcontramap f g = Log (runLog g . f)

newtype Tab a f = Tab { runTab :: Log f -> a }

instance FFunctor (Tab a) where
  ffmap f g = Tab (runTab g . fcontramap f)

instance FFunctor Box where
  ffmap f (Box p s) = Box (f p) s
