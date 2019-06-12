module Text.Parsnip.Location
( location
, Location(..)
, located
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe

-- deliberately lazy
data Location = Location
  { locationLine   :: Int
  , locationColumn :: Int
  , locationSource :: ByteString
  } deriving Show

-- | O(n) in the size of the input bytestring to actually use one.
-- but you don't do that very often, do you?
location :: ByteString -> Int -> Location
location bs j = Location (B.count '\n' before) (j - k) $ B.takeWhile (/='\n') $ B.drop k bs where
  before = B.take j bs
  k = fromMaybe 0 $ B.elemIndexEnd '\n' before

-- | Use this til we get a pretty printer in here
located :: Location -> String -> String
located (Location l c bs) msg = Prelude.unlines
  [ show l ++ ":" ++ show c ++ " " ++ msg
  , ls
  , show l ++ " | " ++ B.unpack bs
  , ls ++ Prelude.replicate c ' ' ++ "^"
  ] where ls = Prelude.replicate (length (show l) + 1) ' ' ++ "|"
