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
import           MusicBrainz.API.JSON
import           MusicBrainz.Data.Edit

import qualified MusicBrainz.Data as MB

create :: Form Text MusicBrainz (RefObject (Revision ReleaseGroup))
create =
  fmap RefObject $ runApi $
    withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> (ReleaseGroupTree
                    <$> "release_group" .: releaseGroup
                    <*> pure ""))
