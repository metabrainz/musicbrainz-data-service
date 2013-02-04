{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Recording where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Common as Common
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Recording)
tree = RecordingTree <$> "recording" .: undefined
                     <*> relationships
                     <*> annotation
                     <*> undefined
                     <*> undefined


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Recording))
create = Common.create tree


--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Recording))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Recording)
viewRevision = runApi $
  MB.viewRevision <$> revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships recordingRevision


--------------------------------------------------------------------------------
recordingRevision :: Form Text MusicBrainz (Ref (Revision Recording))
recordingRevision = revision


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = Common.viewAnnotation recordingRevision


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Recording))
update = Common.update tree


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Recording))
merge = Common.merge


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Recording))
getRevision = Common.getRevision
