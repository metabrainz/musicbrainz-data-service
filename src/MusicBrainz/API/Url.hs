{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Url
    ( findLatest
    , create
    , viewRevision
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Create as Create
import qualified MusicBrainz.API.FindLatest as FindLatest
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Url))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Url))
create = Create.create $ UrlTree <$> "url" .: url


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Label)
viewRevision = runApi $
  MB.viewRevision <$> "revision" .: revision
