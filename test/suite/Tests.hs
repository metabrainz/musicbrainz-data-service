{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Parsers
import qualified Handlers

import Test.MusicBrainz

main :: IO ()
main = testRunner
  [ testGroup "Parsers" Parsers.tests
  , testGroup "Handlers" Handlers.tests
  ]
