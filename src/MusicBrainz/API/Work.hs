{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Work
    ( findLatest
    , viewRevision
    , create
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.Create as Create
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.Data as MB
import           MusicBrainz.API.JSON

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Work))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Work)
viewRevision = runApi $
  MB.viewRevision <$> "revision" .: revision


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Work))
create = Create.create $
  WorkTree <$> "work" .: work
           <*> pure mempty
           <*> pure mempty
           <*> pure ""
           <*> pure mempty -- (Set.fromList <$> "iswcs" .: listOf ("iswc" :. iswc))
  where
    iswcField =
      validate (\input -> case input ^? iswc of
                  Nothing -> Error "Invalid ISWC"
                  Just i -> return i) nonEmptyText

