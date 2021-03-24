{-# language TypeApplications #-}
{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language MagicHash #-}
module Text.Parsnip.Internal.Simple
( SimpleResult(..)
, relative
, absolute
) where

import Data.ByteString
import GHC.Prim
import GHC.Types
import Text.Parsnip.Internal.Parser
import Text.Parsnip.Internal.Private

--------------------------------------------------------------------------------
-- * Simple parsers
--------------------------------------------------------------------------------

data SimpleResult a
  = SimpleOK a {-# unpack #-} !Int
  | SimpleFail {-# unpack #-} !Int

relative :: forall s a. KnownBase s => (ByteString -> SimpleResult a) -> Parser s a
relative = case reflectBase @s of
  !(Base x g q r) -> \f -> Parser \p s -> case f $ mkBS (x `plusAddr#` minusAddr# p q) g (minusAddr# r p) of
    SimpleOK a (I# i) -> OK a (plusAddr# p i) s
    SimpleFail (I# i) -> Fail (plusAddr# p i) s
{-# inline relative #-}

absolute :: forall s a. KnownBase s => (ByteString -> Int -> SimpleResult a) -> Parser s a
absolute = case reflectBase @s of
  !(Base x g q r) -> \f -> Parser \p s -> case f (mkBS x g (minusAddr# r q)) $ I# (minusAddr# p q) of
    SimpleOK a (I# i) -> OK a (plusAddr# p i) s
    SimpleFail (I# i) -> Fail (plusAddr# p i) s
{-# inline absolute #-}
