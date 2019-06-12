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
{-# language BlockArguments #-}
{-# language ViewPatterns #-}
{-# options_ghc -O2 #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.Primitive.ByteArray
import Data.Kind
import Data.String
import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.ForeignPtr
import GHC.Arr
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
  fmap f (Parser m) = Parser \ p s -> mapResult f (m p s)
  {-# inline fmap #-}
  b <$ Parser m = Parser \ p s -> case m p s of
    OK a q t -> OK b q t
    Fail q t -> Fail q t
  {-# inline (<$) #-}

instance Applicative (Parser s) where
  pure a = Parser \ p s -> OK a p s
  {-# inline pure #-}
  Parser m <*> Parser n = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK f q t -> mapResult f (n q t)
  {-# inline (<*>) #-}
  Parser m *> Parser n = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK f q t -> n q t
  {-# inline (*>) #-}
  Parser m <* Parser n = Parser \p s -> case m p s of
    OK a q t -> setResult a (n q t)
    p -> p
  {-# inline (<*) #-}

instance Monad (Parser s) where
  Parser m >>= f = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK a q t -> runParser (f a) q t
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}
  fail _ = Parser Fail
  {-# inline fail #-}

instance Alternative (Parser s) where
  Parser m <|> Parser n = Parser \ p s -> case m p s of
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


instance PrimMonad (Parser s) where
  type PrimState (Parser s) = s
  primitive f = Parser \p s -> case f s of
    (# t, a #) -> OK a p t

data SimpleResult a
  = SimpleOK a {-# unpack #-} !Int
  | SimpleFail {-# unpack #-} !Int

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

-- let GHC figure it how to worker wrapper these
relative :: forall s a. ReifiesBase s => (ByteString -> SimpleResult a) -> Parser s a 
relative f = Parser \p s -> case f $ mkBS (x `plusAddr#` minusAddr# p q) g (minusAddr# r p) of
    SimpleOK a (I# i) -> OK a (plusAddr# p i) s
    SimpleFail (I# i) -> Fail (plusAddr# p i) s
  where Base x g q r = reflectBase @s

-- take a parsnip v0 parser and convert it to this form
absolute :: forall s a. ReifiesBase s => (ByteString -> Int -> SimpleResult a) -> Parser s a 
absolute f = Parser \p s -> case f (mkBS x g (minusAddr# r q)) $ I# (minusAddr# p q) of
    SimpleOK a (I# i) -> OK a (plusAddr# p i) s
    SimpleFail (I# i) -> Fail (plusAddr# p i) s
  where Base x g q r = reflectBase @s
  
atEnd :: Parser s Bool
atEnd = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> OK (isTrue# do chr# 0# `eqChar#` c) p t

endOfInput :: Parser s ()
endOfInput = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> (# if isTrue# do chr# 0# `eqChar#` c then Some () else None, p, t #)

breakSubstring :: ReifiesBase s => ByteString -> Parser s ByteString
breakSubstring needle = relative \bs -> case p bs of
    (r, _) -> SimpleOK r (B.length r)
  where p = B.breakSubstring needle

newtype Mark s = Mark (Ptr Word8) -- unexposed, so known valid addresses 
  deriving (Eq,Ord,Show)

pattern Mk :: Addr# -> Mark s
pattern Mk a = Mark (Ptr a)

instance ReifiesBase s => Bounded (Mark s) where
  minBound = Mk l where Base _ _ l _ = reflectBase @s
  maxBound = Mk h where Base _ _ _ h = reflectBase @s

instance ReifiesBase s => Enum (Mark s) where
  fromEnum p = minusMark p minBound
  toEnum (I# i)
    | isTrue# (0# <=# i) && isTrue# (i <=# minusAddr# h l) = Mk (plusAddr# l i)
    | otherwise = error "Mark.toEnum: Out of bounds" 
    where Base _ _ l h = reflectBase @s
  succ (Mk p)
    | isTrue# (ltAddr# p h) = Mk (plusAddr# p 1#)
    | otherwise = error "Mark.succ: Out of bounds"
    where Base _ _ _ h = reflectBase @s
  pred (Mk p)
    | isTrue# (ltAddr# l p) = Mk (plusAddr# p (negateInt# 1#))
    | otherwise = error "Mark.pred: Out of bounds"
    where Base _ _ l _ = reflectBase @s
  enumFrom (Mk p) = ptrs1 p h
    where Base _ _ l h = reflectBase @s
  enumFromTo (Mk p) (Mk q) = ptrs1 p q
  enumFromThen (Mk p) (Mk q)
    | isTrue# (gtAddr# p q) = dptrs p (minusAddr# q p) l
    | otherwise = ptrs p (minusAddr# q p) h
    where Base _ _ l h = reflectBase @s
  enumFromThenTo (Mk p) (Mk q) (Mk r)
    | isTrue# (gtAddr# p q) = dptrs p (minusAddr# q p) r
    | otherwise = ptrs p (minusAddr# q p) r

instance Ix (Mark s) where
  range (Mk p, Mk q) = ptrs1 p q
  unsafeIndex (p,_) r = minusMark r p
  inRange (Mk p, Mk q) (Mk r) = isTrue# (leAddr# p r) && isTrue# (leAddr# r q)
  unsafeRangeSize = uncurry minusMark

ptrs1 :: Addr# -> Addr# -> [Mark s]
ptrs1 l h
  | isTrue# (leAddr# l h) = Mk l : ptrs1 (plusAddr# l 1#) h
  | otherwise = []

ptrs :: Addr# -> Int# -> Addr# -> [Mark s]
ptrs l d h
  | isTrue# (leAddr# l h) = Mk l : ptrs (plusAddr# l d) d h
  | otherwise = []

dptrs :: Addr# -> Int# -> Addr# -> [Mark s]
dptrs h d l
  | isTrue# (leAddr# l h) = Mark (Ptr h) : ptrs (plusAddr# h d) d l
  | otherwise = []

minusMark :: Mark s -> Mark s -> Int
minusMark (Mk p) (Mk q) = I# (minusAddr# p q)

mark :: Parser s (Mark s) 
mark = Parser \p s -> OK (Mark (Ptr p)) p s

release :: Mark s -> Parser s ()
release (Mark (Ptr q)) = Parser \_ s -> OK () q s

snip :: forall s. ReifiesBase s => Mark s -> Mark s -> Parser s ByteString
snip (Mark (Ptr i)) (Mark (Ptr j)) = Parser \p s -> OK (mkBS x g (minusAddr# i j)) p s 
  where Base x g q r = reflectBase @s

try :: Parser s a -> Parser s a
try (Parser m) = Parser $ \p s -> case m p s of 
  OK a q t -> OK a q t
  Fail q t -> Fail p t

input :: ReifiesBase s => Parser s ByteString
input = absolute \b _ -> SimpleOK b 0

snipping :: forall s a. ReifiesBase s => Parser s a -> Parser s ByteString
snipping (Parser m) = Parser \p s -> case m p s of
    (# o, q, t #) -> (# setOption (mkBS (b `plusAddr#` minusAddr# p r) g (minusAddr# q p)) o, q, t #)
  where Base b g r _ = reflectBase @s

snipPos :: ReifiesBase s => Int -> Int -> Parser s ByteString
snipPos i j = absolute $ \bs _ -> SimpleOK (B.take (j - i) $ B.drop i bs) 0

foreign import ccall unsafe "string.h memchr" c_memchr :: Addr# -> CInt -> CSize -> IO (Ptr ())
foreign import ccall unsafe "string.h strncmp" c_strncmp :: Addr# -> Addr# -> CSize -> IO CInt
foreign import ccall unsafe "string.h strlen" c_strlen :: Addr# -> IO CSize
foreign import ccall unsafe "string.h strlen" pure_strlen :: Addr# -> CSize

cint :: CInt -> Int#
cint (fromIntegral -> I# i) = i

csize :: CSize -> Int#
csize (fromIntegral -> I# i) = i

-- super unsafe
litN :: Addr# -> CSize -> Parser s ByteString
litN q n = Parser \p s -> case io (c_strncmp p q n) s of
    (# t, 0 #) -> OK (unsafeLiteralByteStringN q n) (p `plusAddr#` csize n) s
    (# t, _ #) -> Fail p t

lit :: Addr# -> Parser s ByteString
lit q = litN q (pure_strlen q)

literalForeignPtrContents :: ForeignPtrContents
literalForeignPtrContents = unsafeDupablePerformIO $ primitive \s -> case newByteArray# 0# s of
  (# t, a #) -> (# t, PlainPtr a #)
-- {-# noinline literalForeignPtrContents #-}

unsafeLiteralForeignPtr :: Addr# -> ForeignPtr Word8
unsafeLiteralForeignPtr addr = ForeignPtr addr undefined -- literalForeignPtrContents

unsafeLiteralByteStringN :: Addr# -> CSize -> ByteString
unsafeLiteralByteStringN p n = PS (unsafeLiteralForeignPtr p) 0 (fromIntegral n)

unsafeLiteralByteString :: Addr# -> ByteString
unsafeLiteralByteString p = unsafeLiteralByteStringN p (pure_strlen p)

-- perhaps this interface is a little low level. hrmm
instance a ~ ByteString => IsString (Parser s a) where
  fromString "" = pure B.empty
  fromString xs = Parser \p s -> case sizeofMutableByteArray# ba of
    n -> case io (c_strncmp (mutableByteArrayContents# ba) p (fromIntegral $ I# n)) s of
      (# t, i #)
        | i /= 0    -> Fail p t
        | otherwise -> OK bs (plusAddr# p n) t
    where MutableByteArray ba = pinnedByteArrayFromString0 xs
          bs = B.PS (ForeignPtr (mutableByteArrayContents# ba) (PlainPtr ba)) 0 (I# (sizeofMutableByteArray# ba))

-- !(*@#U missing primitive
mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# arr = byteArrayContents# (unsafeCoerce# arr)

-- hacked to return in any region
pinnedByteArrayFromString0 :: String -> MutableByteArray RealWorld
pinnedByteArrayFromString0 xs = pinnedByteArrayFromStringN0 (length xs) xs

pinnedByteArrayFromStringN0 :: Int -> String -> MutableByteArray RealWorld
pinnedByteArrayFromStringN0 n ys = unsafeDupablePerformIO do
  marr <- newPinnedByteArray (n+1)
  let go !ix [] = if ix == n
        then writeByteArray marr ix (0 :: Word8)
        else fail "pinnedByteArrayFromStringN: list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeByteArray marr ix (B.c2w x)
          go (ix + 1) xs
        else fail "pinnedByteArrayFromStringN: list length greater than specified size"
  go 0 ys
  pure marr
  --unsafeFreezeByteArray marr

take :: forall s. ReifiesBase s => Int -> Parser s ByteString
take (I# i) = Parser \p s ->
  if isTrue# (minusAddr# r p <# i)
  then Fail p s
  else OK (PS (ForeignPtr (b `plusAddr#` minusAddr# p q) g) 0 (I# i)) (plusAddr# p i) s
  where Base b g q r = reflectBase @s

-- | We can do this two ways, this way is O(1) but needs ReifiesBase.
drop :: forall s. ReifiesBase s => Int -> Parser s ()
drop (I# i) = Parser \p s ->
  if isTrue# (minusAddr# r p <# i)
  then Fail p s
  else OK () (plusAddr# p i) s
  where Base _ _ _ r = reflectBase @s

-- | Linear time, but no @ReifiesBase@ dependency.
drop0 :: Int -> Parser s ()
drop0 n@(I# i) = Parser \p s -> case io (c_memchr p 0 (fromIntegral n)) s of
  (# t, Ptr q #) -> if isTrue# (q `eqAddr#` nullAddr#)
    then OK () (plusAddr# p i) t
    else Fail p s

io :: IO a -> State# s -> (# State# s, a #)
io = unsafeCoerce#

---------------------------------------------------------------------------------------
-- * Running parsers and positioning
---------------------------------------------------------------------------------------

deriving instance Show Location

data Base s = Base Addr# ForeignPtrContents Addr# Addr#

bytes :: Base s -> ByteString
bytes (Base b g p q) = mkBS b g (minusAddr# q p)

mkBS :: Addr# -> ForeignPtrContents -> Int# -> ByteString
mkBS b g l = PS (ForeignPtr b g) 0 (I# l)

class ReifiesBase (s :: Type) where
  reflectBase :: Base s

pos :: forall s. ReifiesBase s => Parser s Int
pos = Parser \ p s -> OK (I# (minusAddr# p q)) p s where
  Base _ _ q _ = reflectBase @s
{-# inline pos #-}

data Wrap s a = Wrap (ReifiesBase s => Proxy# s -> Parser s a)

withBase :: (ReifiesBase s => Proxy# s -> Parser s a)
         -> Base s -> Proxy# s -> Parser s a
withBase f x y = magicDict (Wrap f) x y

finish :: Base s -> Addr# -> Option a -> Either Location a
finish (Base b g q r) p = \case
  Some a -> Right a
  None   -> Left (location (mkBS b g (minusAddr# r q)) (I# (minusAddr# p q)))

canonicalPS :: ByteString -> ByteString
canonicalPS (PS fp o l) = PS (fp `plusForeignPtr` o) 0 l

-- | holy extensions batman
parse :: (forall s. ReifiesBase s => Parser s a) -> ByteString -> Either Location a
parse m bs@(PS (ForeignPtr b g) (I# o) (I# len)) = unsafeDupablePerformIO $
  B.useAsCString bs \(Ptr p) -> -- now it is null terminated
    IO \s -> let base = Base (plusAddr# b o) g p (plusAddr# p len) in
      case runParser (withBase (\_ -> m) base proxy#) p s of
        (# m, q, t #) -> (# t, finish base q m #)

main :: IO ()
main = do
  print do parse pos ""
  print do parse empty "money" :: Either Location Int
  print do parse (lit "yo"#) "yo"
  print do parse (breakSubstring "ne") "money"
