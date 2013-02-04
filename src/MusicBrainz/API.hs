{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.API
    ( -- * Parsers
      annotation
    , mbid
    , name
    , nonEmptyText

      -- ** Entity reference parsers
    , edit
    , editorRef
    , revision, revisionRef
    , coreRef

      -- ** Entities
    , artist
    , label
    , releaseGroup
    , url
    , work

      -- ** Tree data
    , aliases
    , relationships

      -- * Running API Calls
    , runApi

      -- * Prelabelled fields
    , editor
    ) where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Traversable (sequenceA)
import Network.URI (parseURI)
import Text.Digestive

import Data.Set ()
import qualified Data.Set as Set
import qualified Data.Text as T

import MusicBrainz hiding (mbid, labelCode, coreRef)
import qualified MusicBrainz as MB

import MusicBrainz.Data

--------------------------------------------------------------------------------
{-| Parse an MBID parameter. -}
mbid :: Monad m => Form Text m (MBID a)
mbid = "mbid" .: validate parse (string Nothing)
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
workTypeRef :: Form Text MusicBrainz (Maybe (Ref WorkType))
workTypeRef = optionalRef "Invalid work type reference"


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
labelTypeRef :: Form Text MusicBrainz (Maybe (Ref LabelType))
labelTypeRef = optionalRef "Invalid label type reference"


--------------------------------------------------------------------------------
languageRef :: Form Text MusicBrainz (Maybe (Ref Language))
languageRef = optionalRef "Invalid language  reference"


--------------------------------------------------------------------------------
releaseGroupTypeRef :: ResolveReference (ReleaseGroupType a)
  => Form Text MusicBrainz (Maybe (Ref (ReleaseGroupType a)))
releaseGroupTypeRef = optionalRef "Invalid release group type reference"


--------------------------------------------------------------------------------
artist :: Form Text MusicBrainz Artist
artist = Artist <$> name
                <*> sortName
                <*> comment
                <*> beginDate
                <*> endDate
                <*> ended
                <*> "gender" .: genderRef
                <*> "type" .: artistTypeRef
                <*> "country" .: countryRef


--------------------------------------------------------------------------------
label :: Form Text MusicBrainz Label
label = Label <$> name
              <*> sortName
              <*> comment
              <*> beginDate
              <*> endDate
              <*> ended
              <*> "type" .: labelTypeRef
              <*> "code" .: labelCode
              <*> "country" .: countryRef
  where labelCode = check "Label codes must be positive and at most 5 digits"
                      (maybe True (\i -> i > 0 && i < 100000)) $
                        optionalStringRead "Invalid integer" Nothing

--------------------------------------------------------------------------------
releaseGroup :: Form Text MusicBrainz ReleaseGroup
releaseGroup = ReleaseGroup <$> name
                            <*> comment
                            <*> "artist_credit" .: artistCreditRef
                            <*> "primary_type" .: releaseGroupTypeRef
                            <*> pure mempty  -- Require's #52 to be fixed


--------------------------------------------------------------------------------
url :: Form Text MusicBrainz Url
url = Url <$> uri
  where
    uri = validate (maybe (Error "Invalid URI") Success . parseURI) $
      string Nothing


--------------------------------------------------------------------------------
work :: Form Text MusicBrainz Work
work = Work <$> name
            <*> comment
            <*> "type" .: workTypeRef
            <*> "language" .: languageRef


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


--------------------------------------------------------------------------------
name, sortName :: Monad m => Form Text m Text
name     = "name" .: nonEmptyText
sortName = "sort-name" .: nonEmptyText

--------------------------------------------------------------------------------
comment :: Monad m => Form Text m Text
comment = "comment" .: text Nothing


--------------------------------------------------------------------------------
partialDate, beginDate, endDate :: Monad m => Form Text m PartialDate
partialDate = PartialDate <$> "year" .: optionalStringRead "Could not read integer" Nothing
                          <*> "month" .: optionalStringRead "Could not read integer" Nothing
                          <*> "day" .: optionalStringRead "Could not read integer" Nothing
beginDate = "begin-date" .: partialDate
endDate = "end-date" .: partialDate


--------------------------------------------------------------------------------
ended :: Monad m => Form Text m Bool
ended = "ended" .: bool Nothing


--------------------------------------------------------------------------------
editor :: Form Text MusicBrainz (Ref Editor)
editor = "editor" .: editorRef


--------------------------------------------------------------------------------
edit :: Form Text MusicBrainz (Ref Edit)
edit = "edit" .: editRef
  where editRef = ref "Invalid edit reference"


--------------------------------------------------------------------------------
revision :: ResolveReference (Revision a) => Form Text MusicBrainz (Ref (Revision a))
revision = "revision" .: revisionRef
  where revisionRef = ref "Invalid revision reference"


--------------------------------------------------------------------------------
revisionRef :: ResolveReference (Revision a) => Form Text MusicBrainz (Ref (Revision a))
revisionRef = ref "Invalid revision reference"


--------------------------------------------------------------------------------
aliases :: ResolveReference (AliasType a) => Form Text MusicBrainz (Set.Set (Alias a))
aliases = Set.fromList <$> "aliases" .: listOf (const alias) Nothing
  where
    alias = Alias <$> "name" .: nonEmptyText
                  <*> "sort-name" .: nonEmptyText
                  <*> beginDate
                  <*> endDate
                  <*> ended
                  <*> "type" .: aliasTypeRef
                  <*> "locale" .: locale
                  <*> "primary-for-locale" .: bool Nothing
    locale = validate (\t -> if T.null t then Success Nothing else Success (Just t)) $
      text Nothing
    aliasTypeRef = optionalRef "Invalid alias type reference"


--------------------------------------------------------------------------------
relationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
relationships =
    Set.unions <$> "relationships" .: sequenceA [ ArtistRelationship `via` "artist"
                                                , LabelRelationship `via` "label"
                                                , RecordingRelationship `via` "recording"
                                                , ReleaseRelationship `via` "release"
                                                , ReleaseGroupRelationship `via` "release-group"
                                                , UrlRelationship `via` "url"
                                                , WorkRelationship `via` "work"
                                                ]
  where
    f `via` key = Set.fromList <$> key .: listOf (const $ relationshipsOf f) Nothing

    relationshipsOf f =
      f <$> "target" .: coreRef
        <*> (Relationship <$> "type" .: ref "Invalid relationship type"
                          <*> "attributes" .: attributes
                          <*> beginDate
                          <*> endDate
                          <*> ended)

    attributes =
      Set.fromList <$> listOf (const (ref "Invalid relationship attribute type")) Nothing


--------------------------------------------------------------------------------
coreRef :: (RefSpec a ~ MBID a, ResolveReference a) => Form Text MusicBrainz (Ref a)
coreRef = validateM resolveMbid (string Nothing)
  where
    resolveMbid s = case s ^? MB.mbid of
      Just mbid' -> maybe (Error "Could not resolve MBID") Success <$> resolveReference mbid'
      Nothing -> pure $ Error "Could not parse MBID"


--------------------------------------------------------------------------------
annotation :: Form Text MusicBrainz Text
annotation = "annotation" .: (text Nothing)
