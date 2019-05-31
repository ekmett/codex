{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language Trustworthy #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--

module Foreign.Const.Marshal.Array
( peekAnArray
, peekAnArray0
, withConstArray
, withConstArray0
, withConstArrayLen
, withConstArrayLen0
, copyAnArray
, moveAnArray
, lengthAnArray0
, advanceConstPtr
, advanceAPtr
) where

import Data.Coerce
import Data.Type.Coercion
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Data.Const.Unsafe

peekAnArray :: forall a p. (APtr p, Storable a) => Int -> p a -> IO [a]
peekAnArray = gcoerceWith (unsafePtrCoercion @p @a) (coerce (peekArray @a))
{-# inline peekAnArray #-}

peekAnArray0 :: forall a p. (APtr p, Storable a, Eq a) => a -> p a -> IO [a]
peekAnArray0 = gcoerceWith (unsafePtrCoercion @p @a) (coerce (peekArray0 @a))
{-# inline peekAnArray0 #-}

withConstArray :: forall a b. Storable a => [a] -> (ConstPtr a -> IO b) -> IO b
withConstArray = coerce (withConstArray @a @b)
{-# inline withConstArray #-}

withConstArray0 :: forall a b. Storable a => a -> [a] -> (ConstPtr a -> IO b) -> IO b
withConstArray0 = coerce (withConstArray0 @a @b)
{-# inline withConstArray0 #-}

withConstArrayLen :: forall a b. Storable a => [a] -> (Int -> ConstPtr a -> IO b) -> IO b
withConstArrayLen = coerce (withConstArrayLen @a @b)
{-# inline withConstArrayLen #-}

withConstArrayLen0 :: forall a b. Storable a => a -> [a] -> (Int -> ConstPtr a -> IO b) -> IO b
withConstArrayLen0 = coerce (withConstArrayLen0 @a @b)
{-# inline withConstArrayLen0 #-}

copyAnArray :: forall a p. (APtr p, Storable a) => Ptr a -> p a -> Int -> IO ()
copyAnArray = gcoerceWith (unsafePtrCoercion @p @a) (coerce (copyArray @a))
{-# inline copyAnArray #-}

moveAnArray :: forall a p. (APtr p, Storable a) => Ptr a -> p a -> Int -> IO ()
moveAnArray = gcoerceWith (unsafePtrCoercion @p @a) (coerce (moveArray @a))
{-# inline moveAnArray #-}

lengthAnArray0 :: forall a p. (APtr p, Storable a, Eq a) => a -> p a -> IO Int
lengthAnArray0 = gcoerceWith (unsafePtrCoercion @p @a) (coerce (lengthArray0 @a))
{-# inline lengthAnArray0 #-}

advanceConstPtr :: forall a p. (APtr p, Storable a) => p a -> Int -> ConstPtr a
advanceConstPtr =  gcoerceWith (unsafePtrCoercion @p @a) (coerce (advancePtr @a))
{-# inline advanceConstPtr #-}

advanceAPtr :: forall a p. (APtr p, Storable a) => p a -> Int -> p a
advanceAPtr =  gcoerceWith (unsafePtrCoercion @p @a) (coerce (advancePtr @a))
{-# inline advanceAPtr #-}
