{-# language MagicHash #-}
{-# language BlockArguments #-}
{-# language UnboxedTuples #-}
module Text.Parsnip.Char8
( satisfy
, char
, anyChar
) where

import GHC.Types
import GHC.Prim
import Text.Parsnip.Internal.Parser
import Text.Parsnip.Internal.Result

--------------------------------------------------------------------------------
-- * Char parsers
--------------------------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser s Char
satisfy f = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> if isTrue# (chr# 0# `neChar#` c) && f (C# c)
    then OK (C# c) (plusAddr# p 1#) t
    else Fail p t

char :: Char -> Parser s Char
char x@(C# c) = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c' #) -> if isTrue# (eqChar# c c')
    then OK x (plusAddr# p 1#) t
    else Fail p t

anyChar :: Parser s Char
anyChar = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> if isTrue# (chr# 0# `neChar#` c)
    then OK (C# c) (plusAddr# p 1#) t
    else Fail p t

