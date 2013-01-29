{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Url
    ( findLatest
    , create
    , viewRevision
    ) where

import           Control.Applicative
import           Data.Monoid (mempty)
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
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Url))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Url))
create = Create.create $ (UrlTree <$> "url" .: url
                                  <*> pure mempty)


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Url)
viewRevision = runApi $
  MB.viewRevision <$> revision
