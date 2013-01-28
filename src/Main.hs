module Main where

import MusicBrainz.Service (serviceInitAutomatic)

import Snap (serveSnaplet, defaultConfig)

--------------------------------------------------------------------------------
main :: IO ()
main = serveSnaplet defaultConfig serviceInitAutomatic
