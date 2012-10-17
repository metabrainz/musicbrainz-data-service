{-# LANGUAGE OverloadedStrings #-}

module MusicBrainz.API
    ( -- * Parsers
      mbid
    ) where

import Data.Text (Text)
import Text.Digestive

import MusicBrainz (MBID, parseMbid)

--------------------------------------------------------------------------------
{-| Parse an MBID parameter. -}
mbid :: Monad m => Form Text m (MBID a)
mbid = validate parse (string Nothing)
  where parse s = case parseMbid s of
          Just mbid' -> Success mbid'
          Nothing -> Error "Could not parse MBID"
