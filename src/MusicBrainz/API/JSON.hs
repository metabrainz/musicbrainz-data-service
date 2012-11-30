{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.JSON
    ( ) where

import Control.Lens (view, by)
import Data.Aeson

import MusicBrainz

--------------------------------------------------------------------------------
instance ToJSON (MBID a) where
  toJSON = toJSON . view (by mbid)


--------------------------------------------------------------------------------
instance (ToJSON a, ToJSON (Ref a)) => ToJSON (CoreEntity a) where
  toJSON e = object [ "mbid" .= coreRef e
                    , "data" .= coreData e
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
instance ToJSON ReleaseGroup where
  toJSON ReleaseGroup{..} = object
    [ "name" .= releaseGroupName
    , "comment" .= releaseGroupComment
    , "primary_type" .= releaseGroupPrimaryType
    , "artist_credit" .= releaseGroupArtistCredit
    ]


--------------------------------------------------------------------------------
instance ToJSON PartialDate where
  toJSON (PartialDate y m d) = object [ "year" .= y
                                      , "month" .= m
                                      , "day" .= d
                                      ]


--------------------------------------------------------------------------------
instance ToJSON (Ref ArtistCredit) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref Artist) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref ArtistType) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref Country) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref Editor) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref Gender) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref Label) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref LabelType) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref ReleaseGroup) where
  toJSON = toJSON . dereference


--------------------------------------------------------------------------------
instance ToJSON (Ref (ReleaseGroupType a)) where
  toJSON = toJSON . dereference
