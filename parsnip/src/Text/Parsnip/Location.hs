module Text.Parsnip.Location
( location
, Location(..)
, located
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe

data Location = Location Int Int ByteString

location :: ByteString -> Int -> Location
location bs j = Location (B.count '\n' before) (j - start) content where
  (before, after) = B.splitAt j bs
  start = fromMaybe 0 $ B.elemIndexEnd '\n' before
  end = maybe (B.length bs) (B.length before +) (B.elemIndex '\n' after)
  content = B.take (end - start) $ B.drop start bs

-- | Use this til we get a pretty printer in here
located :: Location -> String -> String
located (Location l c bs) msg = Prelude.unlines
  [ show l ++ ":" ++ show c ++ " " ++ msg
  , ls
  , show l ++ " | " ++ B.unpack bs
  , ls ++ Prelude.replicate c ' ' ++ "^"
  ] where ls = Prelude.replicate (length (show l) + 1) ' ' ++ "|"
