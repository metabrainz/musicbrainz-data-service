{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ReleaseGroup where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set

import           MusicBrainz hiding (coreRef)
import           MusicBrainz.API
import           MusicBrainz.API.JSON
import qualified MusicBrainz.API.Common as Common

import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.ReleaseGroup as MB

--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree ReleaseGroup)
tree = ReleaseGroupTree <$> "release-group" .: releaseGroup
                        <*> relationships
                        <*> annotation


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision ReleaseGroup))
create = Common.create tree


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity ReleaseGroup)
viewRevision = runApi $
  MB.viewRevision <$> revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships rgRevision


--------------------------------------------------------------------------------
rgRevision :: Form Text MusicBrainz (Ref (Revision ReleaseGroup))
rgRevision = revision


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = Common.viewAnnotation rgRevision


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision ReleaseGroup))
update = Common.update tree


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision ReleaseGroup))
merge = Common.merge


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision ReleaseGroup))
getRevision = Common.getRevision


--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Map.Map (Ref ReleaseGroup) (CoreEntity ReleaseGroup))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
findByArtist :: Form Text MusicBrainz [CoreEntity ReleaseGroup]
findByArtist = runApi $ MB.findByArtist <$> "artist" .: coreRef
