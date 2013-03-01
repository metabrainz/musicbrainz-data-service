{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Artist where

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

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Map.Map (Ref Artist) (CoreEntity Artist))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Artist))
create = Common.create tree

--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Artist)
tree = ArtistTree <$> "artist" .: artist
                  <*> relationships
                  <*> aliases
                  <*> ipiCodes
                  <*> annotation


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Artist)
viewRevision = runApi $
  MB.viewRevision <$> revision


--------------------------------------------------------------------------------
eligibleForCleanup :: Form Text MusicBrainz EligibleForCleanup
eligibleForCleanup = Common.eligibleForCleanup artistRevision


--------------------------------------------------------------------------------
artistRevision :: Form Text MusicBrainz (Ref (Revision Artist))
artistRevision = revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships artistRevision


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = Common.viewAnnotation artistRevision


--------------------------------------------------------------------------------
viewAliases :: Form Text MusicBrainz (Set.Set (Alias Artist))
viewAliases = Common.viewAliases


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Artist))
merge = Common.merge


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Artist))
update = Common.update tree


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Artist))
getRevision = Common.getRevision


--------------------------------------------------------------------------------
viewIpiCodes :: Form Text MusicBrainz (Map.Map (Ref (Revision Artist)) (Set.Set IPI))
viewIpiCodes = Common.viewIpiCodes
