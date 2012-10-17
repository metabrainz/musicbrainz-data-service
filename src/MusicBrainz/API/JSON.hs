{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.JSON
    ( ) where

import Data.Aeson

import MusicBrainz

instance ToJSON (MBID a) where
  toJSON = toJSON . mbidToString

instance ToJSON a => ToJSON (CoreEntity a) where
  toJSON CoreEntity{..} = object [ "mbid" .= coreMbid
                                 , "data" .= coreData
                                 ]

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

instance ToJSON PartialDate where
  toJSON (PartialDate y m d) = object [ "year" .= y
                                      , "month" .= m
                                      , "day" .= d
                                      ]

instance ToJSON (Ref Gender) where
  toJSON (GenderRef id') = toJSON id'

instance ToJSON (Ref ArtistType) where
  toJSON (ArtistTypeRef id') = toJSON id'

instance ToJSON (Ref Country) where
  toJSON (CountryRef id') = toJSON id'
