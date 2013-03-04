{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Recording where

import           Control.Applicative
import           Control.Lens
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set

import           MusicBrainz hiding (coreRef)
import           MusicBrainz.API
import qualified MusicBrainz.API.Common as Common
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Recording as MB

--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Recording)
tree = RecordingTree <$> "recording" .: recording
                     <*> relationships
                     <*> annotation
                     <*> setOf isrcF
                     <*> setOf puidF
  where
    recording = Recording <$> name
                          <*> comment
                          <*> artistCreditRef
                          <*> duration
    isrcF = validate (maybe (Error "Could not parse ISRC") Success . preview isrc) $ text Nothing
    puidF = validate (maybe (Error "Could not parse PUID") Success . preview puid) $ string Nothing


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Recording))
create = Common.create tree


--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Map.Map (Ref Recording) (CoreEntity Recording))
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


--------------------------------------------------------------------------------
findRecordingTracks :: Form Text MusicBrainz [MB.RecordingUse]
findRecordingTracks = runApi $ MB.findRecordingTracks <$> "recording" .: coreRef


--------------------------------------------------------------------------------
findByArtist :: Form Text MusicBrainz [CoreEntity Recording]
findByArtist = runApi $ MB.findByArtist <$> "artist" .: coreRef
