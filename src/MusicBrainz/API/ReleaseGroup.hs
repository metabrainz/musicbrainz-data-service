{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ReleaseGroup
    ( create
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API

import qualified MusicBrainz.Data as MB

create :: Form Text MusicBrainz (CoreEntity ReleaseGroup)
create = runApi $
  MB.create <$> "editor" .: editorRef
            <*> (ReleaseGroupTree <$> "release_group" .: releaseGroup)
