{-# LANGUAGE OverloadedStrings #-}
module Data.LineWrap.Types
    ( Width(..)
    , frold
    , froldM
    )
where

import           Data.Foldable                  ( foldlM )

import           Data.Text                      ( Text )

newtype Width = Width Int deriving (Show, Eq, Ord)

testInput :: Text
testInput = "AAA BB CC DDDDD"

knuthplassExampleText :: Text
knuthplassExampleText =
    "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face"

frold :: Foldable f => f a -> b -> (b -> a -> b) -> b
frold tb a f = foldl f a tb

froldM :: (Foldable f, Monad m) => f a -> b -> (b -> a -> m b) -> m b
froldM tb a f = foldlM f a tb
