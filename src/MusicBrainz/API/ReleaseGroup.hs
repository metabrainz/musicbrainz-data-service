{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ReleaseGroup
    ( create
    , viewRevision
    ) where

import           Control.Applicative
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import           MusicBrainz.API.JSON
import qualified MusicBrainz.API.Create as Create

import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision ReleaseGroup))
create = Create.create $
  ReleaseGroupTree <$> "release_group" .: releaseGroup
                   <*> pure mempty
                   <*> pure ""


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity ReleaseGroup)
viewRevision = runApi $
  MB.viewRevision <$> revision
