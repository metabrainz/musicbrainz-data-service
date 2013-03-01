{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.API.Common
    ( create
    , eligibleForCleanup
    , findLatest
    , getRevision
    , merge
    , update
    , viewAliases
    , viewAnnotation
    , viewIpiCodes
    , viewRelationships
    ) where

import Prelude hiding (lookup)

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz hiding (coreRef, mbid)
import MusicBrainz.API
import MusicBrainz.API.JSON
import MusicBrainz.Data.Relationship (HoldsRelationships)
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

type MBForm a = Form Text MusicBrainz a

--------------------------------------------------------------------------------
create :: MB.Create a
       => MBForm (Tree a)
       -> MBForm (RefObject (Revision a))
create tree =
  fmap RefObject $ runApi $
    MB.withEdit
      <$> edit
      <*> (MB.create
             <$> editor
             <*> tree)


--------------------------------------------------------------------------------
findLatest :: (RefSpec a ~ MBID a, MB.FindLatest a, MB.ResolveReference a, MB.Merge a)
  => MBForm (Map.Map (Ref a) (CoreEntity a))
findLatest = runApi $ MB.findLatest <$> setOf coreRef


--------------------------------------------------------------------------------
update :: (MB.ResolveReference (Revision a), MB.Update a)
       => MBForm (Tree a)
       -> MBForm (RefObject (Revision a))
update tree =
  fmap RefObject $ runApi $
    MB.withEdit
      <$> edit
      <*> (MB.update
             <$> editor
             <*> revision
             <*> tree)


--------------------------------------------------------------------------------
eligibleForCleanup :: HoldsRelationships a => MBForm (Ref (Revision a)) -> MBForm EligibleForCleanup
eligibleForCleanup revision' = fmap EligibleForCleanup $ runApi $
  MB.eligibleForCleanup <$> revision'


--------------------------------------------------------------------------------
viewRelationships :: HoldsRelationships a => MBForm (Ref (Revision a)) -> MBForm (Set.Set LinkedRelationship)
viewRelationships revision' = runApi $ MB.viewRelationships <$> revision'


--------------------------------------------------------------------------------
merge :: (MB.Merge a, MB.ResolveReference a, MB.ResolveReference (Revision a), RefSpec a ~ MBID a) => MBForm (RefObject (Revision a))
merge = fmap RefObject $ runApi $
    MB.withEdit
      <$> edit
      <*> (MB.merge
             <$> editor
             <*> "source" .: revisionRef
             <*> "target" .: coreRef)


--------------------------------------------------------------------------------
getRevision :: MB.ResolveReference (Revision a) => MBForm (Entity (Revision a))
getRevision = runApi $ MB.getEntity <$> revision


--------------------------------------------------------------------------------
viewAnnotation :: MB.ViewAnnotation a => MBForm (Ref (Revision a)) -> MBForm Annotation
viewAnnotation revision' = fmap Annotation $ runApi $ MB.viewAnnotation <$> revision'


--------------------------------------------------------------------------------
viewAliases :: (MB.ResolveReference (Revision a), MB.ViewAliases a) => Form Text MusicBrainz (Set.Set (Alias a))
viewAliases = runApi $ MB.viewAliases <$> revision


--------------------------------------------------------------------------------
viewIpiCodes :: (MB.ResolveReference (Revision a), MB.ViewIPICodes a)
  => Form Text MusicBrainz (Map.Map (Ref (Revision a)) (Set.Set IPI))
viewIpiCodes = runApi $
  MB.viewIpiCodes <$> (Set.fromList <$> "revisions" .: listOf (const revision) Nothing)
