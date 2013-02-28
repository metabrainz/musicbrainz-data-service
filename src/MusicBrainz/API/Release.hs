{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Release where

import           Control.Applicative
import           Control.Lens
import           Data.Text (Text)
import           Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set

import           MusicBrainz hiding (coreRef, releaseLabel, partialDate)
import           MusicBrainz.API
import qualified MusicBrainz.API.Common as Common
import           MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Release as MB

--------------------------------------------------------------------------------
tree :: Form Text MusicBrainz (Tree Release)
tree = ReleaseTree <$> "release" .: release
                   <*> relationships
                   <*> annotation
                   <*> labels
                   <*> mediums
  where
    release = Release <$> name
                      <*> comment
                      <*> artistCreditRef
                      <*> "release-group" .: coreRef
                      <*> "date" .: partialDate
                      <*> "ccountry" .: countryRef
                      <*> "script" .: scriptRef
                      <*> "language" .: languageRef
                      <*> "packaging" .: releasePackagingRef
                      <*> "status" .: releaseStatusRef
                      <*> "barcode" .: barcodeF
      where
        scriptRef = optionalRef "Invalid script reference"
        releaseStatusRef = optionalRef "Invalid release status reference"
        releasePackagingRef = optionalRef "Invalid country reference"
        barcodeF = validate toBarcode $ (,) <$> "no-barcode" .: bool Nothing
                                            <*> "barcode" .: string Nothing
          where toBarcode (True, _) = Success (Just NoBarcode)
                toBarcode (False, b) =
                  maybe (Error "Invalid barcode")
                        (Success . Just)
                        (b ^? barcode)

    labels = "labels" .: setOf releaseLabel
      where
        releaseLabel = ReleaseLabel <$> "label" .: optionalCoreRef
                                    <*> "catalog-number" .: optionalText Nothing

    mediums = "mediums" .: listOf (const medium) Nothing
      where
        medium = Medium <$> "name" .: text Nothing
                        <*> "format" .: optionalRef "Invalid format reference"
                        <*> "position" .: stringRead "Could not read medium position" Nothing
                        <*> "tracks" .: listOf (const track) Nothing
                        <*> "cdtocs" .: setOf cdtoc
          where
            track = Track <$> name
                          <*> "recording" .: coreRef
                          <*> duration
                          <*> artistCreditRef
                          <*> "number" .: text Nothing
            cdtoc = CdToc <$> "track-offsets" .: listOf (stringRead "Could not read track offset") Nothing
                          <*> "leadout-offset" .: stringRead "Could not read leadout offset" Nothing


--------------------------------------------------------------------------------
findLatest :: Form Text MusicBrainz (MaybeObject (CoreEntity Release))
findLatest = Common.findLatest


--------------------------------------------------------------------------------
viewRevision :: Form Text MusicBrainz (CoreEntity Release)
viewRevision = runApi $
  MB.viewRevision <$> revision


--------------------------------------------------------------------------------
viewRelationships :: Form Text MusicBrainz (Set.Set LinkedRelationship)
viewRelationships = Common.viewRelationships releaseRevision


--------------------------------------------------------------------------------
releaseRevision :: Form Text MusicBrainz (Ref (Revision Release))
releaseRevision = revision


--------------------------------------------------------------------------------
viewAnnotation :: Form Text MusicBrainz Annotation
viewAnnotation = Common.viewAnnotation releaseRevision


--------------------------------------------------------------------------------
update :: Form Text MusicBrainz (RefObject (Revision Release))
update = Common.update tree


--------------------------------------------------------------------------------
merge :: Form Text MusicBrainz (RefObject (Revision Release))
merge = Common.merge


--------------------------------------------------------------------------------
getRevision :: Form Text MusicBrainz (Entity (Revision Release))
getRevision = Common.getRevision


--------------------------------------------------------------------------------
create :: Form Text MusicBrainz (RefObject (Revision Release))
create = Common.create tree


--------------------------------------------------------------------------------
findByLabel :: Form Text MusicBrainz [CoreEntity Release]
findByLabel = runApi $ MB.findByLabel <$> "label" .: coreRef


--------------------------------------------------------------------------------
viewReleaseLabels :: Form Text MusicBrainz (Map.Map (Ref (Revision Release)) (Set.Set ReleaseLabel))
viewReleaseLabels = runApi $ MB.viewReleaseLabels <$> setOf revisionRef
