{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Url where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Common as Common
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB

--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Url)
tree = UrlTree <$> "url" .: url
               <*> relationships


--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Url))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Url))
create = Common.create tree


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Url)
viewRevision = runApi $
  MB.viewRevision <$> revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships urlRevision


--------------------------------------------------------------------------------
urlRevision :: Form Text MusicBrainz (Ref (Revision Url))
urlRevision = revision


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Url))
update = Common.update tree


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Url))
merge = Common.merge


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Url))
getRevision = Common.getRevision
