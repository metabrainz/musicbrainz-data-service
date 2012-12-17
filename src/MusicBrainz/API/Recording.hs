{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Recording
    ( findLatest
    , viewRevision
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Recording))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Recording)
viewRevision = runApi $
  MB.viewRevision <$> "revision" .: revision
