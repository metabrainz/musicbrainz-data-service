{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.JSON
    ( Annotation(..)
    , MaybeObject(..)
    , RefObject(..)
    , EligibleForCleanup(..)
    , TopLevel
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Set (Set)
import Data.Text (Text)
import Network.URI (URI)

import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Text as Text

import MusicBrainz

--------------------------------------------------------------------------------
instance ToJSON (MBID a) where
  toJSON = toJSON . view (re mbid)


--------------------------------------------------------------------------------
instance (ToJSON a, ToJSON (Ref a)) => ToJSON (CoreEntity a) where
  toJSON e = object [ "mbid" .= coreRef e
                    , "data" .= coreData e
                    , "revision" .= coreRevision e
                    ]


--------------------------------------------------------------------------------
instance (ToJSON a, ToJSON (Ref a)) => ToJSON (Entity a) where
  toJSON e = object [ "ref" .= entityRef e
                    , "data" .= entityData e
                    ]


--------------------------------------------------------------------------------
instance ToJSON (Alias a) where
  toJSON Alias{..} = object [ "name" .= aliasName
                            , "sort-name" .= aliasSortName
                            , "locale" .= aliasLocale
                            , "type" .= aliasType
                            , "begin-date" .= aliasBeginDate
                            , "end-date" .= aliasEndDate
                            , "ended" .= aliasEnded
                            , "primary-for-locale" .= aliasPrimaryForLocale
                            ]

--------------------------------------------------------------------------------
instance ToJSON Artist where
  toJSON Artist{..} = object [ "name" .= artistName
                             , "sort-name" .= artistSortName
                             , "comment" .= artistComment
                             , "begin-date" .= artistBeginDate
                             , "end-date" .= artistEndDate
                             , "ended" .= artistEnded
                             , "gender" .= artistGender
                             , "type" .= artistType
                             , "country" .= artistCountry
                             ]


--------------------------------------------------------------------------------
instance ToJSON ArtistCreditName where
  toJSON ArtistCreditName{..} = object [ "artist" .= acnArtist
                                       , "name" .= acnName
                                       , "join-phrase" .= acnJoinPhrase
                                       ]


--------------------------------------------------------------------------------
instance ToJSON ArtistType where
  toJSON ArtistType{..} = object [ "name" .= artistTypeName ]


--------------------------------------------------------------------------------
instance ToJSON Editor where
  toJSON Editor{..} = object [ "name" .= editorName
                             ]


--------------------------------------------------------------------------------
instance ToJSON Gender where
  toJSON Gender{..} = object [ "name" .= genderName ]


--------------------------------------------------------------------------------
instance ToJSON Label where
  toJSON Label{..} = object [ "name" .= labelName
                            , "sort-name" .= labelSortName
                            , "comment" .= labelComment
                            , "begin-date" .= labelBeginDate
                            , "end-date" .= labelEndDate
                            , "ended" .= labelEnded
                            , "type" .= labelType
                            , "label-code" .= labelCode
                            ]


--------------------------------------------------------------------------------
instance ToJSON LinkedRelationship where
  toJSON r = case r of
      ArtistRelationship target rel -> go "artist" target rel
      LabelRelationship target rel -> go "label" target rel
      RecordingRelationship target rel -> go "recording" target rel
      ReleaseRelationship target rel -> go "release" target rel
      ReleaseGroupRelationship target rel -> go "release-group" target rel
      UrlRelationship target rel -> go "url" target rel
      WorkRelationship target rel -> go "work" target rel
    where
      go t target rel = object
        [ "target-type" .= (t :: Text)
        , "target" .= target
        , "type" .= relType rel
        , "begin-date" .= relBeginDate rel
        , "end-date" .= relEndDate rel
        , "ended" .= relEnded rel
        , "attributes" .= relAttributes rel
        ]


--------------------------------------------------------------------------------
instance ToJSON Recording where
  toJSON Recording{..} = object [ "name" .= recordingName
                                , "comment" .= recordingComment
                                , "artist-credit" .= recordingArtistCredit
                                , "duration" .= recordingDuration
                                ]


--------------------------------------------------------------------------------
instance ToJSON Release where
  toJSON Release{..} = object [ "name" .= releaseName
                              , "comment" .= releaseComment
                              , "artist-credit" .= releaseArtistCredit
                              , "duration" .= releaseReleaseGroup
                              , "date" .= releaseDate
                              , "country" .= releaseCountry
                              , "script" .= releaseScript
                              , "language" .= releaseLanguage
                              , "packaging" .= releasePackaging
                              , "status" .= releaseStatus
                              ]


--------------------------------------------------------------------------------
instance ToJSON ReleaseGroup where
  toJSON ReleaseGroup{..} = object
    [ "name" .= releaseGroupName
    , "comment" .= releaseGroupComment
    , "primary-type" .= releaseGroupPrimaryType
    , "artist-credit" .= releaseGroupArtistCredit
    ]


--------------------------------------------------------------------------------
instance ToJSON (Revision a) where
  toJSON r = object
    [ "created-at" .= revisionCreatedAt r ]


--------------------------------------------------------------------------------
instance ToJSON URI where
  toJSON = toJSON . show


--------------------------------------------------------------------------------
instance ToJSON Url where
  toJSON Url{..} = object [ "url" .= urlUrl ]


--------------------------------------------------------------------------------
instance ToJSON Work where
  toJSON Work{..} = object [ "name" .= workName
                           , "comment" .= workComment
                           , "language" .= workLanguage
                           , "type" .= workType
                           ]


--------------------------------------------------------------------------------
instance ToJSON PartialDate where
  toJSON date = let (y, m, d) = date ^. re partialDate
                in object [ "year" .= y
                          , "month" .= m
                          , "day" .= d
                          ]


--------------------------------------------------------------------------------
instance (Referenceable a, ToJSON (RefSpec a)) => ToJSON (Ref a) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
newtype RefObject a = RefObject (Ref a)

instance (Referenceable a, ToJSON (RefSpec a)) => ToJSON (RefObject a) where
  toJSON (RefObject r) = object [ "ref" .= r ]


--------------------------------------------------------------------------------
newtype MaybeObject a = MaybeObject (Maybe a)

instance ToJSON a => ToJSON (MaybeObject a) where
  toJSON (MaybeObject (Just a)) = toJSON a
  toJSON (MaybeObject Nothing)  = object []


--------------------------------------------------------------------------------
instance (ToJSON v, ToJSON (RefSpec k), Referenceable k) => ToJSON (Map.Map (Ref k) v) where
  toJSON = Object . HMap.fromList . Map.toList . Map.mapKeys showRef . Map.map toJSON
    where
      showRef r = case toJSON r of
        String s -> s
        Number n -> Text.pack $ show n
        _ -> error "Reference cannot be used a key"

--------------------------------------------------------------------------------
instance ToJSON ISWC where
  toJSON = toJSON . view (re iswc)


--------------------------------------------------------------------------------
instance ToJSON IPI where
  toJSON = toJSON . view (re ipi)


--------------------------------------------------------------------------------
newtype Annotation = Annotation Text

instance ToJSON Annotation where
  toJSON (Annotation t) = object [ "annotation" .= t ]


--------------------------------------------------------------------------------
newtype EligibleForCleanup = EligibleForCleanup Bool

instance ToJSON EligibleForCleanup where
  toJSON (EligibleForCleanup e) = object [ "eligible" .= e ]


--------------------------------------------------------------------------------
class ToJSON a => TopLevel a

instance (Referenceable a, ToJSON (RefSpec a)) => TopLevel (RefObject a)
instance (ToJSON (RefSpec a), ToJSON a, Referenceable a) => TopLevel (CoreEntity a)
instance (ToJSON (RefSpec a), ToJSON a, Referenceable a) => TopLevel (Entity a)
instance (ToJSON v, ToJSON (RefSpec k), Referenceable k) => TopLevel (Map.Map (Ref k) v)
instance ToJSON a => TopLevel (MaybeObject a)
instance ToJSON a => TopLevel (Set a)
instance TopLevel ()
instance TopLevel Annotation
instance TopLevel EligibleForCleanup
instance TopLevel a => TopLevel [a]
