{-# language TypeApplications #-}
{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
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
import Text.Parsnip.Internal.Reflection
import Text.Parsnip.Internal.Result

--------------------------------------------------------------------------------
-- * Simple parsers
--------------------------------------------------------------------------------

data SimpleResult a
  = SimpleOK a {-# unpack #-} !Int
  | SimpleFail {-# unpack #-} !Int

-- let GHC figure it how to worker wrapper these
relative :: forall s a. ReifiesBase s => (ByteString -> SimpleResult a) -> Parser s a
relative f = Parser \p s -> case f $ mkBS (x `plusAddr#` minusAddr# p q) g (minusAddr# r p) of
    SimpleOK a (I# i) -> OK a (plusAddr# p i) s
    SimpleFail (I# i) -> Fail (plusAddr# p i) s
  where Base x g q r = reflectBase @s
{-# inline relative #-}

-- take a parsnip v0 parser and convert it to this form
absolute :: forall s a. ReifiesBase s => (ByteString -> Int -> SimpleResult a) -> Parser s a
absolute f = Parser \p s -> case f (mkBS x g (minusAddr# r q)) $ I# (minusAddr# p q) of
    SimpleOK a (I# i) -> OK a (plusAddr# p i) s
    SimpleFail (I# i) -> Fail (plusAddr# p i) s
  where Base x g q r = reflectBase @s
{-# inline absolute #-}
