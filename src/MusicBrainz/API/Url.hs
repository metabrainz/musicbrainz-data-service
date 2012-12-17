{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Url
    ( findLatest
    , create
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import           MusicBrainz.Data.Edit

findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Url))
findLatest = FindLatest.findLatest

create :: Form Text MusicBrainz (RefObject (Revision Url))
create =
  fmap RefObject $ runApi $
    withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> (UrlTree <$> "url" .: url))
