{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.LineWrap where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Hyphenation

newtype Ideal = Ideal Int deriving (Show, Eq)
newtype MaxLen = MaxLen Int deriving (Show, Eq)
newtype Threshold = Threshold Int deriving (Show, Eq)

data Score
  = Score Int
  | Inf
  deriving (Show, Eq)

instance Ord Score where
  compare (Score a) (Score b) = compare a b
  compare (Score _) Inf       = LT
  compare Inf (Score _)       = GT
  compare Inf Inf             = EQ

score :: Integral n => Ideal -> MaxLen -> n -> Score
score (Ideal i) (MaxLen m) c = if c < fromIntegral m
  then Score $ (i - fromIntegral c) ^ (2::Int)
  else Inf

scoreOver :: Threshold -> Score -> Bool
scoreOver (Threshold t) (Score s) = s > t
scoreOver _              Inf      = True

-- Hyphenate with Text over String
hyphenateText :: Hyphenator -> Text -> [Text]
hyphenateText h = fmap T.pack . hyphenate h . T.unpack

data Mangled = M
  { _mAccum :: Text
  , _mBreaks :: Seq Int
  , _mRemainder :: Text
  }
  deriving (Show, Eq)
makeClassy ''Mangled

floopM :: Mangled -> Int -> Bool -> Mangled
floopM m b needsHyphen = m
  & mAccum <>~ snoc (if needsHyphen then snoc ln '-' else ln) '\n'
  & mBreaks %~ flip snoc b
  & mRemainder .~ T.dropWhile (== ' ') xs
  where
    (ln,xs) = T.splitAt b (m ^. mRemainder)

finaliseMangle :: Mangled -> Text
finaliseMangle (M broken _ remaining) = broken <> remaining

mangleAtBreaks :: Hyphenator -> Threshold -> Ideal -> MaxLen -> Text -> Mangled
mangleAtBreaks hytor thresh ideal@(Ideal idline) maxl@(MaxLen maxline) t =
  go (T.words t) 0 (M mempty S.empty t) 0 0
  where
    go :: [Text] -> Int -> Mangled -> Int -> Int -> Mangled
    go []     _    ms _      _     = ms
    go (x:xs) widx ms curlen lnnum =
      let
        wlen = T.length x
        widx0 = widx + 1

        curwrd = wlen + curlen
      in
        if curwrd >= idline && curwrd <= maxline
        then go xs widx0 (floopM ms (wlen + curlen) False) 0 (lnnum + 1)
        else case (wlen + 1 + curlen) `compare` idline of
          GT -> -- We've exceeded the maximum line length so try to hyphenate
            case hyphenateText hytor x of
              -- cover that empty case! also not sure if possible to hit this?
              []   -> go xs widx0 ms 0 (lnnum + 1)
              -- No hyphenation available, and as we've already
              -- determined that we've exceed the line length, wrap it.
              [_]   -> go xs widx0 ms 0 (lnnum + 1)

              -- We have a hyphenation option, basically do everything again but with bits of a word.
              (h:hs) -> if T.length h + 1 + curlen > maxline -- Check if 'wrd-' + curlen will fit
                then go xs widx0 ms 0 (lnnum + 1) -- We can't hyphenate, so continue as you were.
                else -- We can hyphenate!!
                     -- No attempt is made to combine components of a hyphenation:
                     -- Given: ["beau", "ti", "ful"]
                     -- Could try: "beau-" or "beauti-" ?
                  let
                    -- line plus hyphenated component
                    linelen = curlen + T.length h
                    -- push the remainder of the hyphenated word on to the next line
                    newh = mconcat hs
                  in
                    -- Move the rest of the hypenated word to the front for the next line
                    go (newh : xs) widx0 (floopM ms linelen True) 0 (lnnum + 1)                  

          -- Under max line length, but nothing follows so don't mark it and we're done.
          LT | null xs -> ms

          -- Check the score of the current word and if we're too far
          -- from the end then don't bother scoring and keep moving.
          _ -> if scoreOver thresh $ score ideal maxl curlen
               then go xs widx0 ms (curwrd + 1) lnnum
               else go xs widx0 (floopM ms (curwrd + 1) False) (curwrd + 1) lnnum
