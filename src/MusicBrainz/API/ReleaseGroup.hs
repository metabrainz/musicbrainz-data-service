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
import           MusicBrainz.Data.Edit

import qualified MusicBrainz.Data as MB

create :: Form Text MusicBrainz (Ref (Revision ReleaseGroup))
create =
  runApi $
    withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> (ReleaseGroupTree
                    <$> "release_group" .: releaseGroup
                    <*> pure ""))
