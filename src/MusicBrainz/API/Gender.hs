{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Gender ( add ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import MusicBrainz
import MusicBrainz.API
import qualified MusicBrainz.Data as MB

add :: Form Text MusicBrainz (Entity Gender)
add = runApi $ MB.add <$> (Gender <$> "name" .: nonEmptyText)
