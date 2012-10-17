{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module MusicBrainz.API
    ( -- * Parsers
      mbid

      -- ** Entity reference parsers
    , editorRef

      -- ** Entities
    , artist
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import qualified Data.Text as T

import MusicBrainz

--------------------------------------------------------------------------------
{-| Parse an MBID parameter. -}
mbid :: Monad m => Form Text m (MBID a)
mbid = validate parse (string Nothing)
  where parse s = case parseMbid s of
          Just mbid' -> Success mbid'
          Nothing -> Error "Could not parse MBID"

nonEmptyText :: Monad m => Form Text m Text
nonEmptyText =
  check "Text must be a non-empty string" (not . T.null) (text Nothing)

artistTypeRef :: Monad m => Form Text m (Maybe (Ref ArtistType))
artistTypeRef = fmap ArtistTypeRef <$> optionalStringRead "Invalid ArtistType reference" Nothing

countryRef :: Monad m => Form Text m (Maybe (Ref Country))
countryRef = fmap CountryRef <$> optionalStringRead "Invalid Country reference" Nothing

genderRef :: Monad m => Form Text m (Maybe (Ref Gender))
genderRef = fmap GenderRef <$> optionalStringRead "Invalid Gender reference" Nothing

editorRef :: Monad m => Form Text m (Ref Editor)
editorRef = EditorRef <$> stringRead "Invalid Editor reference" Nothing

artist :: Monad m => Form Text m Artist
artist = Artist <$> "name" .: nonEmptyText
                <*> "sort-name" .: nonEmptyText
                <*> "comment" .: text Nothing
                <*> pure emptyDate
                <*> pure emptyDate
                <*> "ended" .: bool Nothing
                <*> "gender" .: genderRef
                <*> "type" .: artistTypeRef
                <*> "country" .: countryRef
