{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Gender ( add ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.Data.Gender
import           MusicBrainz.API

add :: Form Text MusicBrainz (Entity Gender)
add = runApi $ addGender <$> (Gender <$> "name" .: nonEmptyText)
