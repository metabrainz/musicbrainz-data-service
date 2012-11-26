{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API
    ( -- * Parsers
      mbid

      -- ** Entity reference parsers
    , editorRef

      -- ** Entities
    , artist
    , releaseGroup

      -- * Running API Calls
    , runApi
    ) where

import Control.Applicative
import Control.Lens
import Data.Text (Text)
import Text.Digestive

import qualified Data.Text as T

import MusicBrainz hiding (mbid)
import qualified MusicBrainz as MB

import MusicBrainz.Data

--------------------------------------------------------------------------------
{-| Parse an MBID parameter. -}
mbid :: Monad m => Form Text m (MBID a)
mbid = validate parse (string Nothing)
  where parse s = case s ^? MB.mbid of
          Just mbid' -> Success mbid'
          Nothing -> Error "Could not parse MBID"


--------------------------------------------------------------------------------
nonEmptyText :: Monad m => Form Text m Text
nonEmptyText =
  check "Text must be a non-empty string" (not . T.null) (text Nothing)


--------------------------------------------------------------------------------
artistCreditRef :: Monad m => Form Text m (Ref ArtistCredit)
artistCreditRef = validate (const $ Error "artistCreditRef cannot be implemented until digestive-functors #52 is fixed") $
  text Nothing


--------------------------------------------------------------------------------
artistTypeRef :: Form Text MusicBrainz (Maybe (Ref ArtistType))
artistTypeRef = optionalRef "Invalid artist type reference"


--------------------------------------------------------------------------------
countryRef :: Form Text MusicBrainz (Maybe (Ref Country))
countryRef = optionalRef "Invalid country reference"


--------------------------------------------------------------------------------
editorRef :: Form Text MusicBrainz (Ref Editor)
editorRef = ref "Invalid editor reference"


--------------------------------------------------------------------------------
genderRef :: Form Text MusicBrainz (Maybe (Ref Gender))
genderRef = optionalRef "Invalid gender reference"


--------------------------------------------------------------------------------
releaseGroupTypeRef :: ResolveReference (ReleaseGroupType a)
  => Form Text MusicBrainz (Maybe (Ref (ReleaseGroupType a)))
releaseGroupTypeRef = optionalRef "Invalid release group type reference"


--------------------------------------------------------------------------------
artist :: Form Text MusicBrainz Artist
artist = Artist <$> "name" .: nonEmptyText
                <*> "sort-name" .: nonEmptyText
                <*> "comment" .: text Nothing
                <*> pure emptyDate
                <*> pure emptyDate
                <*> "ended" .: bool Nothing
                <*> "gender" .: genderRef
                <*> "type" .: artistTypeRef
                <*> "country" .: countryRef


--------------------------------------------------------------------------------
releaseGroup :: Form Text MusicBrainz ReleaseGroup
releaseGroup = ReleaseGroup <$> "name" .: nonEmptyText
                            <*> "comment" .: text Nothing
                            <*> "artist_credit" .: artistCreditRef
                            <*> "primary_type" .: releaseGroupTypeRef


--------------------------------------------------------------------------------
runApi :: (Monad m, Functor m) => Form v m (m b) -> Form v m b
runApi = validateM (fmap Success)


--------------------------------------------------------------------------------
optionalRef :: (ResolveReference a, Read (RefSpec a), Show (RefSpec a))
  => Text -> Form Text MusicBrainz (Maybe (Ref a))
optionalRef e = validateM (maybe (return $ Success Nothing) resolveOptionalRefSpec) $
  optionalStringRead e Nothing


--------------------------------------------------------------------------------
ref :: (ResolveReference a, Read (RefSpec a), Show (RefSpec a))
  => Text -> Form Text MusicBrainz (Ref a)
ref e = validateM resolveRefSpec $ stringRead e Nothing


--------------------------------------------------------------------------------
resolveOptionalRefSpec :: ResolveReference a => RefSpec a -> MusicBrainz (Result Text (Maybe (Ref a)))
resolveOptionalRefSpec = resolveRefSpec' Just


--------------------------------------------------------------------------------
resolveRefSpec :: ResolveReference a => RefSpec a -> MusicBrainz (Result Text (Ref a))
resolveRefSpec = resolveRefSpec' id


--------------------------------------------------------------------------------
resolveRefSpec' :: ResolveReference a =>
  (Ref a -> b) -> RefSpec a -> MusicBrainz (Result Text b)
resolveRefSpec' ret r = do
  resolved <- resolveReference r
  case resolved of
    Nothing -> return $ Error "Reference could not be resolved"
    Just ref' -> return $ Success $ ret ref'
