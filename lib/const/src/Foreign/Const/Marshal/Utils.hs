{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}

module Foreign.Const.Marshal.Utils
  ( withConst
  , newConst
  , copyBytesAt
  , moveBytesAt
  , maybeNewA
  , maybePeekA
  , maybeWithA
  ) where

import Data.Coerce
import Data.Type.Coercion
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

import Data.Const.Unsafe

withConst :: forall a b. Storable a => a -> (ConstPtr a -> IO b) -> IO b
withConst = coerce $ withConst @a @b

newConst :: forall a. Storable a => a -> IO (ConstPtr a)
newConst = coerce $ new @a

copyBytesAt :: forall a p. APtr p => Ptr a -> p a -> Int -> IO ()
copyBytesAt = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ copyBytes @a

moveBytesAt :: forall a p. APtr p => Ptr a -> p a -> Int -> IO ()
moveBytesAt = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ moveBytes @a

maybeNewA :: forall a b p.  APtr p => (a -> IO (p b)) -> Maybe a -> IO (p b)
maybeNewA = gcoerceWith (unsafePtrCoercion @p @b) $ coerce $ maybeNew @a @b

maybePeekA :: forall a b p. APtr p => (p a -> IO b) -> p a -> IO (Maybe b)
maybePeekA = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ maybePeek @a @b

maybeWithA :: forall a b c p. APtr p => (a -> (p b -> IO c) -> IO c) -> Maybe a -> (p b -> IO c) -> IO c
maybeWithA = gcoerceWith (unsafePtrCoercion @p @b) $ coerce $ maybeWith @a @b @c
