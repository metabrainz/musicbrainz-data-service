{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Artist
    ( findLatest
    , create
    ) where

import           Control.Applicative
import           Data.Text (Text)
import           Text.Digestive

import qualified MusicBrainz.Data.Artist

import           MusicBrainz
import           MusicBrainz.API
import qualified MusicBrainz.API.FindLatest as FindLatest

findLatest :: Monad m => Form Text m (MusicBrainz (Maybe (CoreEntity Artist)))
findLatest = FindLatest.findLatest

create :: Monad m => Form Text m (MusicBrainz (CoreEntity Artist))
create = do
  MusicBrainz.Data.Artist.create <$> "editor" .: editorRef
                                 <*> "artist" .: artist
