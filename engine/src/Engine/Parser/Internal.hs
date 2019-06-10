{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language DeriveTraversable #-}

module Engine.Parser.Internal
( Parser(..)
, Result(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Data.String
import Prelude hiding (takeWhile, dropWhile, take, drop)

-- tiniest parser
data Result a
   = OK a {-# unpack #-} !Int
   | Fail
   deriving (Show, Functor, Foldable, Traversable)

-- non-backtracking parser
newtype Parser a = Parser { runParser :: ByteString -> Int -> Result a }
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \_ -> OK a
  {-# inline pure #-}
  Parser m <*> Parser n = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> case n bs j of
      OK b k -> OK (a b) k
      Fail -> Fail
  {-# inline (<*>) #-}
  Parser m *> Parser n = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK _ j -> n bs j
  {-# inline (*>) #-}
  Parser m <* Parser n = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> case n bs j of
      Fail -> Fail
      OK _ k -> OK a k
  {-# inline (<*) #-} 
  liftA2 f (Parser m) (Parser n) = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> case n bs j of
      Fail -> Fail
      OK b k -> OK (f a b) k

instance Monad Parser where
  Parser m >>= f = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> runParser (f a) bs j
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

instance MonadFail Parser where
  fail _ = Parser $ \_ _ -> Fail
  {-# inline fail #-}

instance Alternative Parser where
  empty = Parser $ \_ _ -> Fail
  {-# inline empty #-}
  Parser m <|> Parser n = Parser $ \bs i -> case m bs i of
    Fail -> n bs i
    r@OK{} -> r
  {-# inline (<|>) #-}

instance MonadPlus Parser where
  mzero = empty
  {-# inline mzero #-}
  mplus = (<|>)
  {-# inline mplus #-}

instance a ~ ByteString => IsString (Parser a) where
  fromString s = Parser $ \ bs i ->
    if needle `C.isPrefixOf` B.unsafeDrop i bs
    then OK needle (i + B.length needle)
    else Fail
    where needle = fromString s
  {-# inline fromString #-}
