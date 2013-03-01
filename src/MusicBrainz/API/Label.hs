{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Label where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Common as Common
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import           MusicBrainz.Data.Label ()

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Map.Map (Ref Label) (CoreEntity Label))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Label))
create = Common.create tree


--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Label)
tree = LabelTree <$> "label" .: label
                 <*> relationships
                 <*> aliases
                 <*> ipiCodes
                 <*> annotation


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Label)
viewRevision = runApi $
  MB.viewRevision <$> revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships labelRevision


--------------------------------------------------------------------------------
labelRevision :: Form Text MusicBrainz (Ref (Revision Label))
labelRevision = revision


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = Common.viewAnnotation labelRevision


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Label))
update = Common.update tree


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Label))
merge = Common.merge


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Label))
getRevision = Common.getRevision


--------------------------------------------------------------------------------
eligibleForCleanup :: Form Text MusicBrainz EligibleForCleanup
eligibleForCleanup = Common.eligibleForCleanup labelRevision


--------------------------------------------------------------------------------
viewAliases :: Form Text MusicBrainz (Set.Set (Alias Label))
viewAliases = Common.viewAliases


--------------------------------------------------------------------------------
viewIpiCodes :: Form Text MusicBrainz (Map.Map (Ref (Revision Label)) (Set.Set IPI))
viewIpiCodes = Common.viewIpiCodes
