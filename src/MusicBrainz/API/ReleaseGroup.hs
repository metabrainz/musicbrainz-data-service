{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ReleaseGroup
    ( findLatest
    , create
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified MusicBrainz.Data.ReleaseGroup

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest

findLatest :: Monad m => Form Text m (MusicBrainz (Maybe (CoreEntity ReleaseGroup)))
findLatest = FindLatest.findLatest

create :: Monad m => Form Text m (MusicBrainz (CoreEntity ReleaseGroup))
create = do
  MusicBrainz.Data.ReleaseGroup.create
    <$> "editor" .: editorRef
    <*> "release_group" .: releaseGroup
