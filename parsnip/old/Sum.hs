{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language UnboxedSums #-}
{-# language StandaloneDeriving #-}
{-# language UnboxedTuples #-}
{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language BangPatterns #-}
{-# language ForeignFunctionInterface #-}
{-# language KindSignatures #-}
{-# language UnliftedFFITypes #-}
{-# language TypeApplications #-}
{-# language AllowAmbiguousTypes #-}
{-# language ViewPatterns #-}
{-# options_ghc -O2 #-}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Internal as B
import Data.Primitive.ByteArray
import Data.Kind
import Data.String
import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.ForeignPtr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import GHC.Word
import System.IO.Unsafe

import Text.Parsnip.Location

type Option a = (# a | (##) #)

pattern Some :: a -> Option a
pattern Some a = (# a | #)

pattern None :: Option a
pattern None = (# | (##) #)

{-# complete Some, None #-}

mapOption :: (a -> b) -> Option a -> Option b
mapOption f (Some a) = Some $! f a
mapOption f None = None
{-# inline mapOption #-}

setOption :: b -> Option a -> Option b
setOption b (Some a) = Some b
setOption b None = None
{-# inline setOption #-}

type Result s a = (# Option a, Addr#, State# s #)

-- Worker wrapper? Who dat?

newtype Parser s a = Parser
 { runParser :: Addr# -> State# s -> Result s a
 }

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

instance Functor (Parser s) where
  fmap f (Parser m) = Parser $ \ p s -> mapResult f (m p s)
  {-# inline fmap #-}
  b <$ Parser m = Parser $ \ p s -> case m p s of
    OK a q t -> OK b q t
    Fail q t -> Fail q t
  {-# inline (<$) #-}

--adjust :: Addr# -> Addr# -> Int# -> Int#
--adjust p q i = minusAddr# p q +# i -- note this is adding a negative

instance Applicative (Parser s) where
  pure a = Parser $ \ p s -> OK a p s
  {-# inline pure #-}
  Parser m <*> Parser n = Parser $ \p s -> case m p s of
    Fail q t -> Fail q t
    OK f q t -> mapResult f (n q t)
  {-# inline (<*>) #-}
  Parser m *> Parser n = Parser $ \p s -> case m p s of
    Fail q t -> Fail q t
    OK f q t -> n q t
  {-# inline (*>) #-}
  Parser m <* Parser n = Parser $ \p s -> case m p s of
    OK a q t -> setResult a (n q t)
    p -> p
  {-# inline (<*) #-}

instance Monad (Parser s) where
  Parser m >>= f = Parser $ \p s -> case m p s of
    Fail q t -> Fail q t
    OK a q t -> runParser (f a) q t
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}
  fail _ = Parser Fail
  {-# inline fail #-}

instance Alternative (Parser s) where
  Parser m <|> Parser n = Parser $ \ p s -> case m p s of
    Fail _ t -> m p t
    OK a q t -> OK a q t
  {-# inline (<|>) #-}
  empty = Parser Fail
  {-# inline empty #-}

instance MonadPlus (Parser s) where
  mplus = (<|>)
  {-# inline mplus #-}
  mzero = empty
  {-# inline mzero #-}

-- foreign import ccall unsafe "string.h memcmp" c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "string.h memcmp" c_memcmp :: Addr# -> Addr# -> CSize -> IO CInt -- State# s -> (# State# s, CInt #)

rest :: forall s. ReifiesBase s => Parser s ByteString
rest = Parser $ \p -> OK (PS (ForeignPtr p guts) 0 (I# (minusAddr# end p))) p where
  Base (ForeignPtr q guts) end = reflectBase @s


{-
-- perhaps this interface is a little low level. hrmm
instance a ~ ByteString => IsString (Parser s a) where
  fromString "" = pure B.empty
  fromString xs = Parser $ \i p s -> case sizeOfByteArray# ba of
      n -> if isTrue# (i <# n) then Fail p s
        else case c_memcmp (byteArrayContents# ba) p (fromIntegral (I# n)) of 
           IO f -> case f s of
             (# t, i #)
               | i /= 0 -> Fail p t
               | otherwise -> OK bs (p `plusAddr#` n) s
    where ba = pinnedPrimArrayFromString xs
          bs = B.PS (ForeignPtr (byteArrayContents# ba) (PlainPtr ba)) 0 (I# (sizeOfByteArray# ba)))
-}
 
pinnedByteArrayFromString :: String -> ByteArray
pinnedByteArrayFromString xs = byteArrayFromListN (length xs) xs

pinnedByteArrayFromStringN :: Int -> String -> ByteArray
pinnedByteArrayFromStringN n ys = runST $ do
  marr <- newPinnedByteArray n
  let go !ix [] = if ix == n
        then return ()
        else fail "pinnedByteArrayFromStringN: list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeByteArray marr ix (B.c2w x)
          go (ix + 1) xs
        else fail "pinnedByteArrayFromStringN: list length greater than specified size"
  go 0 ys
  unsafeFreezeByteArray marr

---------------------------------------------------------------------------------------
-- * Running parsers and positioning
---------------------------------------------------------------------------------------

deriving instance Show Location

-- | Reifying the base position as a dictionary avoids passing it at all down branches
-- that do not use it, letting GHC know it is a constant.

data Base s = Base {-# unpack #-} !(ForeignPtr Word8) Addr#

class ReifiesBase (s :: Type) where
  reflectBase :: Base s

-- extract current location
pos :: forall s. ReifiesBase s => Parser s Int
pos = Parser $ \ p s -> OK (I# (minusAddr# p b)) p s where
  Base (ForeignPtr b _) _ = reflectBase @s
{-# inline pos #-}

data Wrap s a = Wrap (ReifiesBase s => Proxy# s -> Parser s a)

withBase :: (ReifiesBase s => Proxy# s -> Parser s a)
         -> Base s -> Proxy# s -> Parser s a
withBase f x y = magicDict (Wrap f) x y

finish :: Addr# -> Addr# -> ByteString -> Option a -> Either Location a
finish p q bs = \case
  Some a -> Right a
  None -> Left (location bs (I# (minusAddr# p q)))

-- | holy extensions batman
parse :: (forall s. ReifiesBase s => Parser s a) -> ByteString -> Either Location a
parse m bs@(PS fp0 o (I# len)) = case plusForeignPtr fp0 o of
  fp -> unsafeDupablePerformIO $
    withForeignPtr fp $ \ (Ptr p) ->
      IO $ \s -> case runParser (withBase (\_ -> m) (Base fp (p `plusAddr#` len)) proxy#) p s of
        (# m, q, t #) -> (# t, finish p q bs m #)

main :: IO ()
main = do
  print $ parse pos ""
  print $ do parse empty "money" :: Either Location Int
  print $ parse rest "yo" 
