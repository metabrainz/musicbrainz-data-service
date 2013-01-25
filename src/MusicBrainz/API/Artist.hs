{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Artist
    ( findLatest
    , create
    , viewRevision
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Create as Create
import qualified MusicBrainz.API.FindLatest as FindLatest
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Artist))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Artist))
create = Create.create $
  ArtistTree <$> "artist" .: artist
             <*> pure Set.empty
             <*> aliases
             <*> pure Set.empty
             <*> pure ""


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Artist)
viewRevision = runApi $
  MB.viewRevision <$> revision
