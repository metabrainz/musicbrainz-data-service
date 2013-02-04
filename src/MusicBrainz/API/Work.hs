{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Work
    ( findLatest
    , viewRevision
    , create
    , update
    , viewAliases
    , viewAnnotation
    , eligibleForCleanup
    , viewRelationships
    , merge
    , getRevision
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import           MusicBrainz hiding (coreRef)
import           MusicBrainz.API
import qualified MusicBrainz.API.Create as Create
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.API.Update as Update
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB
import           MusicBrainz.API.JSON

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Work))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Work)
viewRevision = runApi $ MB.viewRevision <$> revision


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Work))
create = Create.create tree


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Work))
update = Update.update tree


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
viewAliases = runApi $
  MB.viewAliases <$> workRevision


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = fmap Annotation $ runApi $ MB.viewAnnotation <$> workRevision


--------------------------------------------------------------------------------
eligibleForCleanup :: Form Text MusicBrainz EligibleForCleanup
eligibleForCleanup = fmap EligibleForCleanup $ runApi $
  MB.eligibleForCleanup <$> workRevision


--------------------------------------------------------------------------------
workRevision :: Form Text MusicBrainz (Ref (Revision Work))
workRevision = revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = runApi $ MB.viewRelationships <$> workRevision


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Work))
merge = fmap RefObject $ runApi $
    MB.withEdit
      <$> edit
      <*> (MB.merge
             <$> editor
             <*> "source" .: revisionRef
             <*> "target" .: coreRef)


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Work))
getRevision = runApi $ MB.getEntity <$> workRevision
