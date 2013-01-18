{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.API.FindLatest
    ( findLatest
    ) where

import Prelude hiding (lookup)

import Control.Applicative
import Data.Text (Text)
import Data.Traversable (traverse)
import Text.Digestive

import qualified MusicBrainz.Data as MB

import MusicBrainz.API
import MusicBrainz.API.JSON
import MusicBrainz hiding (mbid)

--------------------------------------------------------------------------------
findLatest :: (RefSpec a ~ MBID a, MB.FindLatest a, MB.ResolveReference a, MB.Merge a)
  => Form Text MusicBrainz (MaybeObject (CoreEntity a))
findLatest = validateM lookup $ "mbid" .: mbid
  where
    lookup mbid' =
      (Success . MaybeObject) <$> (MB.resolveReference mbid' >>= traverse MB.findLatest)
