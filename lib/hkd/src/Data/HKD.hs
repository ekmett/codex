{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language Trustworthy #-}
{-# language GADTs #-}
{-# language CPP #-}
#ifndef HLINT
{-# language LambdaCase #-}
{-# language EmptyCase #-}
#endif
-- | "Higher-Kinded Data" such as it is
module Data.HKD 
  ( type (~>)
  -- * Functor
  , FFunctor(..)
  -- * Contravariant
  , FContravariant(..)
  -- * Foldable
  , FFoldable(..)
  , flength
  , ftraverse_
  , ffor_
  -- * Traversable
  , FTraversable(..)
  , ffoldMapDefault
  , ffmapDefault
  , ffor
  -- * Utilities
  , Log(..)
  , Tab(..)
  , indexLog
  , Element(..)
  ) where

import Control.Applicative
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity
import Data.Functor.Sum
import qualified Data.Monoid as Monoid
import Data.Proxy
import GHC.Generics

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f

type f ~> g = forall a. f a -> g a

-- * FFunctor

class FFunctor t where
  ffmap :: (f ~> g) -> t f -> t g

instance (Functor f, FFunctor g) => FFunctor (Compose f g) where
  ffmap f = Compose . fmap (ffmap f) . getCompose

instance (FFunctor f, FFunctor g) => FFunctor (Product f g) where
  ffmap f (Pair g h) = Pair (ffmap f g) (ffmap f h)

instance (FFunctor f, FFunctor g) => FFunctor (Sum f g) where
  ffmap f (InL g) = InL (ffmap f g)
  ffmap f (InR h) = InR (ffmap f h)

instance (Functor f, FFunctor g) => FFunctor (f :.: g) where
  ffmap f = Comp1 . fmap (ffmap f) . unComp1

instance (FFunctor f, FFunctor g) => FFunctor (f :*: g) where
  ffmap f (g :*: h) = ffmap f g :*: ffmap f h

instance (FFunctor f, FFunctor g) => FFunctor (f :+: g) where
  ffmap f (L1 g) = L1 (ffmap f g)
  ffmap f (R1 h) = R1 (ffmap f h)

instance FFunctor (Const a) where
  ffmap _ (Const a) = Const a

instance FFunctor (K1 i a) where
  ffmap _ (K1 a) = K1 a

instance FFunctor Proxy where
  ffmap _ Proxy = Proxy

instance FFunctor U1 where
  ffmap _ U1 = U1

instance FFunctor V1 where
#ifndef HLINT
  ffmap _ = \case
#endif

class FFoldable t where
  ffoldMap :: Monoid m => (forall a. f a -> m) -> t f -> m

  flengthAcc :: Int -> t f -> Int
  flengthAcc acc t = acc + Monoid.getSum (ffoldMap (\_ -> Monoid.Sum 1) t)

flength :: FFoldable t => t f -> Int
flength = flengthAcc 0 

data M m where
  M :: m a -> M m

instance Applicative m => Semigroup (M m) where
  M m <> M n = M (m *> n)

instance Applicative m => Monoid (M m) where
  mempty = M (pure ())

ftraverse_ :: (FFoldable t, Applicative m) => (forall a. f a -> m b) -> t f -> m ()
ftraverse_ k tf = case ffoldMap (M . k) tf of
  M mx -> () <$ mx

ffor_ :: (FFoldable t, Applicative m) => t f -> (forall a. f a -> m b) -> m ()
ffor_ tf k = ftraverse_ k tf

instance FFoldable (Const a) where
  ffoldMap _ = mempty
  flengthAcc = const

instance FFoldable (K1 i a) where
  ffoldMap _ = mempty
  flengthAcc = const

instance FFoldable U1 where
  ffoldMap _ = mempty
  flengthAcc = const

instance FFoldable Proxy where
  ffoldMap _ = mempty
  flengthAcc = const

instance FFoldable V1 where
#ifndef HLINT
  ffoldMap _ = \case
  flengthAcc _ = \case
#endif

instance (Foldable f, FFoldable g) => FFoldable (Compose f g) where
  ffoldMap f = foldMap (ffoldMap f) . getCompose

instance (FFoldable f, FFoldable g) => FFoldable (Product f g) where
  ffoldMap f (Pair g h) = ffoldMap f g <> ffoldMap f h
  flengthAcc f (Pair g h) = f `flengthAcc` g `flengthAcc` h

instance (FFoldable f, FFoldable g) => FFoldable (Sum f g) where
  ffoldMap f (InL g) = ffoldMap f g
  ffoldMap f (InR h) = ffoldMap f h

instance (Foldable f, FFoldable g) => FFoldable (f :.: g) where
  ffoldMap f = foldMap (ffoldMap f) . unComp1

instance (FFoldable f, FFoldable g) => FFoldable (f :*: g) where
  ffoldMap f (g :*: h) = ffoldMap f g <> ffoldMap f h
  flengthAcc acc (g :*: h) = acc `flengthAcc` g `flengthAcc` h

instance (FFoldable f, FFoldable g) => FFoldable (f :+: g) where
  ffoldMap f (L1 g) = ffoldMap f g
  ffoldMap f (R1 h) = ffoldMap f h
  flengthAcc acc (L1 g) = flengthAcc acc g
  flengthAcc acc (R1 g) = flengthAcc acc g

-- * FTraversable

class (FFoldable t, FFunctor t) => FTraversable t where
  ftraverse :: Applicative m => (forall a. f a -> m (g a)) -> t f -> m (t g)
ffmapDefault :: FTraversable t =>  (f ~> g) -> t f -> t g
ffmapDefault k = runIdentity . ftraverse (Identity . k) 

ffoldMapDefault :: (FTraversable t, Monoid m) =>  (forall a. f a -> m) -> t f -> m
ffoldMapDefault k = getConst . ftraverse (Const . k) 

ffor :: (FTraversable t, Applicative m) => t f -> (forall a. f a -> m (g a)) -> m (t g)
ffor tf k = ftraverse k tf

instance FTraversable (Const a) where
  ftraverse _ (Const a) = pure (Const a)

instance FTraversable (K1 i a) where
  ftraverse _ (K1 a) = pure (K1 a)

instance FTraversable Proxy where
  ftraverse _ Proxy = pure Proxy

instance FTraversable U1 where
  ftraverse _ U1 = pure U1

instance FTraversable V1 where
#ifndef HLINT
  ftraverse _ = \case
#endif

instance (Traversable f, FTraversable g) => FTraversable (Compose f g) where
  ftraverse f = fmap Compose . traverse (ftraverse f) . getCompose

instance (FTraversable f, FTraversable g) => FTraversable (Product f g) where
  ftraverse f (Pair g h) = Pair <$> ftraverse f g <*> ftraverse f h

instance (FTraversable f, FTraversable g) => FTraversable (Sum f g) where
  ftraverse f (InL g) = InL <$> ftraverse f g
  ftraverse f (InR h) = InR <$> ftraverse f h

instance (Traversable f, FTraversable g) => FTraversable (f :.: g) where
  ftraverse f = fmap Comp1 . traverse (ftraverse f) . unComp1

instance (FTraversable f, FTraversable g) => FTraversable (f :*: g) where
  ftraverse f (g :*: h) = (:*:) <$> ftraverse f g <*> ftraverse f h

instance (FTraversable f, FTraversable g) => FTraversable (f :+: g) where
  ftraverse f (L1 g) = L1 <$> ftraverse f g
  ftraverse f (R1 h) = R1 <$> ftraverse f h

-- * FContravariant

class FContravariant t where
  fcontramap :: (f ~> g) -> t g -> t f

instance FContravariant (Const a) where
  fcontramap _ (Const a) = Const a

instance FContravariant (K1 i a) where
  fcontramap _ (K1 a) = K1 a

instance FContravariant Proxy where
  fcontramap _ Proxy = Proxy

instance FContravariant U1 where
  fcontramap _ U1 = U1

instance FContravariant V1 where
#ifndef HLINT
  fcontramap _ = \case
#endif


instance (Functor f, FContravariant g) => FContravariant (Compose f g) where
  fcontramap f = Compose . fmap (fcontramap f) . getCompose

instance (FContravariant f, FContravariant g) => FContravariant (Product f g) where
  fcontramap f (Pair g h) = Pair (fcontramap f g) (fcontramap f h)

instance (FContravariant f, FContravariant g) => FContravariant (Sum f g) where
  fcontramap f (InL g) = InL (fcontramap f g)
  fcontramap f (InR h) = InR (fcontramap f h)

instance (Functor f, FContravariant g) => FContravariant (f :.: g) where
  fcontramap f = Comp1 . fmap (fcontramap f) . unComp1

instance (FContravariant f, FContravariant g) => FContravariant (f :*: g) where
  fcontramap f (g :*: h) = fcontramap f g :*: fcontramap f h

instance (FContravariant f, FContravariant g) => FContravariant (f :+: g) where
  fcontramap f (L1 g) = L1 (fcontramap f g)
  fcontramap f (R1 h) = R1 (fcontramap f h)

-- * Distributive Utilities

newtype Log f = Log { runLog :: forall a. f a -> a }

indexLog :: f a -> Log f -> a
indexLog fa (Log fa2a) = fa2a fa

instance FContravariant Log where
  fcontramap f g = Log (runLog g . f)

newtype Tab a f = Tab { runTab :: Log f -> a }

instance FFunctor (Tab a) where
  ffmap f g = Tab (runTab g . fcontramap f)

newtype Element a f = Element { runElement :: f a }

instance FFunctor (Element a) where
  ffmap f (Element fa) = Element (f fa)

instance FFoldable (Element a) where
  ffoldMap f (Element fa) = f fa
  flengthAcc acc _ = acc + 1
 
instance FTraversable (Element a) where
  ftraverse f (Element g) = Element <$> f g
