{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Recording
    ( findLatest
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.Data as MB
import           MusicBrainz.Data.Edit

findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Recording))
findLatest = FindLatest.findLatest
