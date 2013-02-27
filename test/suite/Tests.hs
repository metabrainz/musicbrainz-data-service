{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Parsers
import qualified Handlers
import qualified Wrappers

import Test.MusicBrainz

main :: IO ()
main = testRunner
  [ testGroup "Parsers" Parsers.tests
  , testGroup "Handlers" Handlers.tests
  , testGroup "API Wrappers" Wrappers.tests
  ]
