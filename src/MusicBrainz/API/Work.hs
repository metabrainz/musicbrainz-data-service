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
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Create as Create
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.API.Update as Update
import qualified MusicBrainz.Data as MB
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
           <*> pure mempty
           <*> pure mempty
           <*> "annotation" .: (text Nothing)
           <*> pure mempty -- (Set.fromList <$> "iswcs" .: listOf ("iswc" :. iswc))
  where
    iswcField =
      validate (\input -> case input ^? iswc of
                  Nothing -> Error "Invalid ISWC"
                  Just i -> return i) nonEmptyText


--------------------------------------------------------------------------------
viewAliases :: Form Text MusicBrainz (Set.Set Alias)
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
