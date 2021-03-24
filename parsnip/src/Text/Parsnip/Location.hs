module Text.Parsnip.Location
( location
, Location(..)
, located
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe

-- | Deliberately lazy, so we don't bother to compute the
-- exact line and column until forced.
data Location = Location
  { locationLine   :: Int
  , locationColumn :: Int
  , locationSource :: ByteString
  } deriving Show

-- | /O(n)/ in the size of the input bytestring to actually use one.
-- If used primarily for error location reporting, then this probably
-- puts the burden in the right place.
location :: ByteString -> Int -> Location
location bs j = Location (B.count '\n' before) (j - k) $ B.takeWhile (/='\n') $ B.drop k bs where
  before = B.take j bs
  k = fromMaybe 0 $ B.elemIndexEnd '\n' before
{-# inline location #-}

-- | Use this til we get a pretty printer in here
located :: Location -> String -> String
located (Location l c bs) msg = Prelude.unlines
  [ show l ++ ":" ++ show c ++ " " ++ msg
  , ls
  , show l ++ " | " ++ B.unpack bs
  , ls ++ Prelude.replicate c ' ' ++ "^"
  ] where ls = Prelude.replicate (length (show l) + 1) ' ' ++ "|"
