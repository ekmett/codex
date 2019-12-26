module Main where

import Test.Tasty

import MessageTest (test_message)

main :: IO ()
main = defaultMain $ testGroup "Language Server"
  [ test_message
  ]
