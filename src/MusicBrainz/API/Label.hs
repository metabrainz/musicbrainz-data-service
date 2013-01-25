{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Label
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
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.API.Create as Create
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB
import           MusicBrainz.Data.Label ()

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Label))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Label))
create = Create.create $
  LabelTree <$> "label" .: label
            <*> pure Set.empty
            <*> aliases
            <*> pure Set.empty
            <*> pure ""


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Label)
viewRevision = runApi $
  MB.viewRevision <$> revision
