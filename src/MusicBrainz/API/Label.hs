module MusicBrainz.API.Label
    ( findLatest
    ) where

import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.Data.Label ()
import qualified MusicBrainz.API.FindLatest as FindLatest

findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Label))
findLatest = FindLatest.findLatest
