{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Editor
    ( register
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.Data.Editor as Editor

register :: Form Text MusicBrainz (Entity Editor)
register = runApi $ Editor.register <$> (Editor <$> name <*> "password" .: nonEmptyText)
