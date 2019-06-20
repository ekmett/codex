{-# language BlockArguments #-}
{-# language DerivingStrategies #-}
{-# language TupleSections #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

-- | Simple Tabulation Hashing
--
-- <https://arxiv.org/abs/1011.5200 The Power of Simple Tabulation Hashing> by
-- Mihai Patrascu, Mikkel Thorup, showed that while simple tabulation hashing
-- is only 3-independent, it has many properties of hash functions with higher
-- levels of independence.
--
-- Note: unlike the claims of Wikipedia that simple tabulation hashing is only
-- suitable to fixed sized data, with a lazy chain of hash tables, it works for
-- anything.

module Data.Tabulation.Hash
  ( HashBuilder
  , hash
  , build
  , STH(..)
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bits
import Data.Char
import Data.Int
import Data.List (unfoldr)
import Data.Primitive.PrimArray
import Data.Void
import Data.Word
import Generics.SOP as SOP
import Generics.SOP.GGP as SOP
import GHC.Generics as G
import GHC.Int
import GHC.Prim
import GHC.Word
import Numeric.Natural
import System.Random.MWC

data Hashes = ByteArray# :- Hashes

hashes :: Hashes
hashes = runST $ create >>= loop where
  loop gen = do
    (PrimArray pa :: PrimArray Word32) <- generatePrimArrayP 256 \_-> uniform gen
    (pa :-) <$> unsafeInterleaveST (loop gen)

-- Word# represents a Word32# result
newtype HashBuilder = HashBuilder { _runHashBuilder :: Hashes -> (# Word#, Hashes #) }
instance Semigroup HashBuilder where
  HashBuilder f <> HashBuilder g = HashBuilder \h -> case f h of
    (# a, h' #) -> case g h' of
      (# b, h'' #) -> (# xor# a b, h'' #)

instance Monoid HashBuilder where
  mempty = HashBuilder \hs -> (# 0##,hs #)

i8 :: Integral a => a -> Int#
i8 a = case fromIntegral a .&. 255 of I# i -> i

-- the monoid to use to combine these is xor
sth8 :: Integral a => a -> HashBuilder
sth8 w = HashBuilder \(h :- hs) -> (# indexWord32Array# h (i8 w), hs #)

hash :: STH a => a -> Word32
hash = build . sth

build :: HashBuilder -> Word32
build (HashBuilder hb) = case hb hashes of
  (# w32, _ #) -> W32# w32

class STH a where
  sth :: a -> HashBuilder
  default sth ::
    ( G.Generic a
    , SOP.GFrom a
    , SOP.All2 STH (SOP.GCode a)
    ) => a -> HashBuilder
  sth = gsth

gsth :: (G.Generic a, SOP.GFrom a, SOP.All2 STH (SOP.GCode a)) => a -> HashBuilder
gsth xs0 = case SOP.lengthSList sop of
    1 -> case sop of
      SOP.Z xs -> products xs
      _ -> error "the impossible happened"
    _ -> sums 0 sop
  where
    SOP.SOP sop = SOP.gfrom xs0
    sums :: SOP.All2 STH xss => Word8 -> SOP.NS (SOP.NP SOP.I) xss -> HashBuilder
    sums !acc (SOP.Z xs) = sth acc <> products xs
    sums acc (SOP.S xss) = sums (acc + 1) xss

    products :: SOP.All STH xs => SOP.NP SOP.I xs -> HashBuilder
    products SOP.Nil = mempty
    products (SOP.I x SOP.:* xs) = sth x <> products xs

instance STH Void where sth = absurd
instance STH () where sth _ = mempty  
instance STH (Proxy a) where sth _ = mempty
instance STH a => STH (Maybe a)
instance STH a => STH [a]
instance (STH a, STH b) => STH (a, b)
instance (STH a, STH b) => STH (Either a b)

-- UTF-8 encoded before feeding to sth
-- this way the common ascii case uses up one hash table
instance STH Char where
  sth c
    | w <= 0x7f = sth8 w
    | w <= 0x7ff
        = sth8 (0xc0 + unsafeShiftR w 6)
       <> sth8 (0x80 + w .&. 0x3f)
    | w <= 0xffff
        = sth8 (0xe0 + unsafeShiftR w 12)
       <> sth8 (0x80 + (unsafeShiftR w 6 .&. 0x3f))
       <> sth8 (0x80 + w .&. 0x3f)
    | otherwise
        = sth8 (0xf0 + unsafeShiftR w 18)
       <> sth8 (0x80 + (unsafeShiftR w 12 .&. 0x3f))
       <> sth8 (0x80 + (unsafeShiftR w 6 .&. 0x3f))
       <> sth8 (0x80 + w .&. 0x3f)
    where w = ord c

instance STH Word8 where
  sth = sth8

instance STH Word16 where
  sth w = sth8 (unsafeShiftR w 8)
       <> sth8 w

instance STH Word32 where
  sth w = sth8 (unsafeShiftR w 24)
       <> sth8 (unsafeShiftR w 16)
       <> sth8 (unsafeShiftR w 8)
       <> sth8 w

instance STH Word64 where
  sth w = sth8 (unsafeShiftR w 56)
       <> sth8 (unsafeShiftR w 48)
       <> sth8 (unsafeShiftR w 40)
       <> sth8 (unsafeShiftR w 32)
       <> sth8 (unsafeShiftR w 24)
       <> sth8 (unsafeShiftR w 16)
       <> sth8 (unsafeShiftR w 8)
       <> sth8 w

instance STH Int64 where sth w = sth (fromIntegral w :: Word64)
instance STH Int32 where sth w = sth (fromIntegral w :: Word32)
instance STH Int16 where sth w = sth (fromIntegral w :: Word16)
instance STH Int8 where sth w = sth (fromIntegral w :: Word8)

-- these use 64 bit hashes consistently so that we yield consistent
-- results for in-range values across platforms
instance STH Word where sth w = sth (fromIntegral w :: Word64)
instance STH Int where sth w = sth (fromIntegral w :: Word64)

instance STH Integer where
  sth n
    | n >= fromIntegral (minBound :: Int32) 
    , n <= fromIntegral (maxBound :: Int32) 
    = sth (0 :: Word8)
   <> sth (fromIntegral n :: Int32)
  sth n
    = sth (1 :: Word8)
   <> sth (fromIntegral (signum n) :: Word8)
   <> sth (fromIntegral $ div (numBits (abs n) + 7) 8 :: Word64)
   <> foldMap sth (unroll (abs n))

instance STH Natural where
  sth n
    | n <= fromIntegral (maxBound :: Word64)
    = sth (0 :: Word8)
   <> sth (fromIntegral n :: Word64)
  sth n
    = sth (1 :: Word8)
   <> sth (fromIntegral $ div (numBits (abs n) + 7) 8 :: Word64)
   <> foldMap sth (unroll (abs n))

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step where
  step 0 = Nothing
  step i = Just (fromIntegral i, unsafeShiftR i 8)

numBits :: (Integral a) => a -> Int
numBits k = findN (div expMax 2) expMax where
  expMax = until (\e -> 2 ^ e > k) (* 2) 1
  findN :: Int -> Int -> Int
  findN lo hi
    | mid == lo = hi
    | 2 ^ mid <= k = findN mid hi
    | otherwise = findN lo mid
    where mid = div (lo + hi) 2

