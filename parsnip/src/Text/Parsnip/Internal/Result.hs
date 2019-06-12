{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
module Text.Parsnip.Internal.Result
( Result, pattern OK, pattern Fail
, mapResult, setResult
) where

import GHC.Prim
import Text.Parsnip.Internal.Option

--------------------------------------------------------------------------------
-- * Result
--------------------------------------------------------------------------------

type Result s a = (# Option a, Addr#, State# s #)

pattern OK :: a -> Addr# -> State# s -> Result s a
pattern OK a p s = (# Some a, p, s #)

pattern Fail :: Addr# -> State# s -> Result s a
pattern Fail p s = (# None, p, s #)
{-# complete OK, Fail #-}

mapResult :: (a -> b) -> Result s a -> Result s b
mapResult f (# o, p, s #) = (# mapOption f o, p, s #)
{-# inline mapResult #-}

setResult :: b -> Result s a -> Result s b
setResult b (# o, p, s #) = (# setOption b o, p, s #)
{-# inline setResult #-}
