{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Work where

import           Control.Applicative
import           Control.Lens
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set

import           MusicBrainz hiding (coreRef)
import           MusicBrainz.API
import qualified MusicBrainz.API.Common as Common
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Work as MB
import           MusicBrainz.API.JSON

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Map.Map (Ref Work) (CoreEntity Work))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Work)
viewRevision = runApi $ MB.viewRevision <$> revision


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Work))
create = Common.create tree


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Work))
update = Common.update tree


--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Work)
tree =
  WorkTree <$> "work" .: work
           <*> relationships
           <*> aliases
           <*> annotation
           <*> (Set.fromList <$> "iswcs" .: listOf (const $ "iswc" .: iswcF) Nothing)
  where
    iswcF =
      validate (\input -> case input ^? iswc of
                  Nothing -> Error "Invalid ISWC"
                  Just i -> return i) nonEmptyText


--------------------------------------------------------------------------------
viewAliases :: Form Text MusicBrainz (Set.Set (Alias Work))
viewAliases = Common.viewAliases


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = Common.viewAnnotation workRevision


--------------------------------------------------------------------------------
eligibleForCleanup :: Form Text MusicBrainz EligibleForCleanup
eligibleForCleanup = Common.eligibleForCleanup workRevision


--------------------------------------------------------------------------------
workRevision :: Form Text MusicBrainz (Ref (Revision Work))
workRevision = revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships workRevision


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Work))
merge = Common.merge


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Work))
getRevision = Common.getRevision


--------------------------------------------------------------------------------
findByArtist :: Form Text MusicBrainz [CoreEntity Work]
findByArtist = runApi $ MB.findByArtist <$> "artist" .: coreRef
