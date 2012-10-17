{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.FindLatest
    ( findLatest
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import qualified MusicBrainz.Data.FindLatest

import MusicBrainz.API
import MusicBrainz
import MusicBrainz.Data.FindLatest (FindLatest)

findLatest :: (FindLatest a, Monad m) => Form Text m (MusicBrainz (Maybe (CoreEntity a)))
findLatest =
  MusicBrainz.Data.FindLatest.findLatest <$> "mbid" .: mbid
