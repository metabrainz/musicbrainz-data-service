{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ArtistType
    ( add
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import MusicBrainz
import MusicBrainz.API
import MusicBrainz.Data.ArtistType

add :: Form Text MusicBrainz (Entity ArtistType)
add = runApi $ addArtistType <$> (ArtistType <$> name)
