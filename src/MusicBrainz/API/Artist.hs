{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Artist
    ( findLatestByMbid
    , create
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import qualified MusicBrainz.Data.Artist

import MusicBrainz.API
import MusicBrainz

findLatestByMbid :: Monad m => Form Text m (MusicBrainz (Maybe (CoreEntity Artist)))
findLatestByMbid =
  MusicBrainz.Data.Artist.findLatestByMbid <$> "mbid" .: mbid

create :: Monad m => Form Text m (MusicBrainz (CoreEntity Artist))
create = do
  MusicBrainz.Data.Artist.create <$> "editor" .: editorRef
                                 <*> "artist" .: artist
