{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.JSON
    ( RefObject(..) ) where

import Control.Lens (view, remit)
import Data.Aeson
import Network.URI (URI)

import MusicBrainz

--------------------------------------------------------------------------------
instance ToJSON (MBID a) where
  toJSON = toJSON . view (remit mbid)


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
instance ToJSON Recording where
  toJSON Recording{..} = object [ "name" .= recordingName
                                , "comment" .= recordingComment
                                , "artist_credit" .= recordingArtistCredit
                                , "duration" .= recordingDuration
                                ]


--------------------------------------------------------------------------------
instance ToJSON Release where
  toJSON Release{..} = object [ "name" .= releaseName
                              , "comment" .= releaseComment
                              , "artist_credit" .= releaseArtistCredit
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
    , "primary_type" .= releaseGroupPrimaryType
    , "artist_credit" .= releaseGroupArtistCredit
    ]


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
  toJSON (PartialDate y m d) = object [ "year" .= y
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
