{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Artist
    ( findLatest
    , create
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import qualified MusicBrainz.Data as Data

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest

findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Artist))
findLatest = FindLatest.findLatest

create :: Form Text MusicBrainz (CoreEntity Artist)
create = runApi $ Data.create
                    <$> editor
                    <*> (ArtistTree <$> "artist" .: artist
                                    <*> pure Set.empty
                                    <*> pure Set.empty
                                    <*> pure Set.empty
                                    <*> pure "")
