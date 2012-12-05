{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Artist
    ( findLatest
    , create
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Set as Set

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.Data as MB
import           MusicBrainz.Data.Edit

findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Artist))
findLatest = FindLatest.findLatest

create :: Form Text MusicBrainz (Ref (Revision Artist))
create =
  runApi $
    withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> (ArtistTree <$> "artist" .: artist
                             <*> pure Set.empty
                             <*> pure Set.empty
                             <*> pure Set.empty
                             <*> pure ""))
