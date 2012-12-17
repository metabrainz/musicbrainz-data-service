{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ReleaseGroup
    ( create
    , viewRevision
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import           MusicBrainz.API.JSON

import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision ReleaseGroup))
create =
  fmap RefObject $ runApi $
    MB.withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> (ReleaseGroupTree
                    <$> "release_group" .: releaseGroup
                    <*> pure ""))


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity ReleaseGroup)
viewRevision = runApi $
  MB.viewRevision <$> "revision" .: revision
