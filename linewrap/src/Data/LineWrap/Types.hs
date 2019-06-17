{-# LANGUAGE OverloadedStrings #-}
module Data.LineWrap.Types where

import Data.Foldable (foldlM)

import Data.Text (Text)

newtype Width = Width Int deriving (Show, Eq, Ord)

testInput :: Text
testInput = "AAA BB CC DDDDD"

knuthplassExampleText :: Text
knuthplassExampleText = "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face"

frold tb a f = foldl f a tb
froldM tb a f = foldlM f a tb
