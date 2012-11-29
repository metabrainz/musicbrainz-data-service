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
import           MusicBrainz.Data.Label ()
import qualified MusicBrainz.Data as Data
import qualified MusicBrainz.API.FindLatest as FindLatest

--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (Maybe (CoreEntity Label))
findLatest = FindLatest.findLatest


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (CoreEntity Label)
create = runApi $ Data.create
                     <$> editor
                     <*> (LabelTree <$> "label" .: label
                                    <*> pure Set.empty
                                    <*> pure Set.empty
                                    <*> pure "")
