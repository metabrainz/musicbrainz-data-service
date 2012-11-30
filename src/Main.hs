module Main where

import MusicBrainz.Service (serviceInit)

import Snap (serveSnaplet, defaultConfig)

--------------------------------------------------------------------------------
main :: IO ()
main = serveSnaplet defaultConfig serviceInit
