{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
module Text.Parsnip.Internal.Option
( Option, pattern Some, pattern None
, mapOption, setOption
) where

--------------------------------------------------------------------------------
-- * Option
--------------------------------------------------------------------------------

-- | Unlifted 'Maybe'
type Option a = (# a | (##) #)

pattern Some :: a -> Option a
pattern Some a = (# a | #)

pattern None :: Option a
pattern None = (# | (##) #)

{-# complete Some, None #-}

mapOption :: (a -> b) -> Option a -> Option b
mapOption f (Some a) = Some $! f a
mapOption _ None = None
{-# inline mapOption #-}

setOption :: b -> Option a -> Option b
setOption b (Some a) = Some b
setOption _ None = None
{-# inline setOption #-}
