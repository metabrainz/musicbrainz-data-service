{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Label
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
import           MusicBrainz.Data.Label ()

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Label))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (Ref (Revision Label))
create =
  runApi $
    withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> (LabelTree <$> "label" .: label
                            <*> pure Set.empty
                            <*> pure Set.empty
                            <*> pure ""))
