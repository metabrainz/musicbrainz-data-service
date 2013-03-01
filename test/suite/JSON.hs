{-# LANGUAGE OverloadedStrings #-}
module JSON (tests) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Monoid (mempty)
import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.ArtistCredit
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor
import Test.MusicBrainz

import MusicBrainz.API.JSON ()

tests :: [Test]
tests =
  [ testGroup "ToJSON Release"
      [ testCase "Full release" $ do
          editor <- testEditor
          r <- autoEdit $ do
            ac <- testArtistCredit editor

            rg <- fmap coreRef . viewRevision =<< create editor
              ReleaseGroupTree
                { releaseGroupData = ReleaseGroup
                    { releaseGroupName = "RG"
                    , releaseGroupComment = ""
                    , releaseGroupArtistCredit = ac
                    , releaseGroupPrimaryType = Nothing
                    , releaseGroupSecondaryTypes = mempty
                    }
                , releaseGroupRelationships = mempty
                , releaseGroupAnnotation = ""
                }

            official <- entityRef <$> add ReleaseStatus { releaseStatusName = "Official" }

            digipak <- entityRef <$> add ReleasePackaging { releasePackagingName = "Digipak" }

            english <- entityRef <$> add Language
              { languageName = "English"
              , languageIsoCode2t = "eng"
              , languageIsoCode2b = "eng"
              , languageIsoCode1 = "en"
              , languageIsoCode3 = "eng"
              }

            latin <- entityRef <$> add Script
              { scriptName = "Latin"
              , scriptIsoNumber = "215"
              , scriptIsoCode = "Latn"
              }

            uk <- entityRef <$> add Country
              { countryIsoCode = "GB"
              , countryName = "United Kingdom"
              }

            return Release { releaseName = "Release"
                           , releaseComment = "Comment"
                           , releaseArtistCredit = ac
                           , releaseReleaseGroup = rg
                           , releaseDate = (Just 2012, Just 2, Just 15) ^?! partialDate
                           , releaseCountry = Just uk
                           , releaseScript = Just latin
                           , releasePackaging = Just digipak
                           , releaseStatus = Just official
                           , releaseBarcode = Just $ "0013964381993" ^?! barcode
                           , releaseLanguage = Just english
                           }

          toJSON r @?=
            object [ "name" .= releaseName r
                   , "comment" .= releaseComment r
                   , "artist-credit" .= releaseArtistCredit r
                   , "release-group" .= releaseReleaseGroup r
                   , "date" .= releaseDate r
                   , "country" .= releaseCountry r
                   , "script" .= releaseScript r
                   , "packaging" .= releasePackaging r
                   , "status" .= releaseStatus r
                   , "barcode" .= releaseBarcode r
                   , "language" .= releaseLanguage r
                   ]
      ]

  , testCase "ToJSON Track" $ do
      editor <- testEditor
      track <- autoEdit $ do
        ac <- testArtistCredit editor
        recording <- fmap coreRef . viewRevision =<< create editor
          RecordingTree { recordingData = Recording { recordingName = "R"
                                                    , recordingComment = mempty
                                                    , recordingArtistCredit = ac
                                                    , recordingDuration = Just 12
                                                    }
                        , recordingRelationships = mempty
                        , recordingAnnotation = mempty
                        , recordingIsrcs = mempty
                        , recordingPuids = mempty
                        }
        return Track { trackName = "Track name"
                     , trackRecording = recording
                     , trackDuration = Just 12345
                     , trackArtistCredit = ac
                     , trackPosition = "4"
                    }
      toJSON track @?=
        object [ "name" .= trackName track
               , "artist-credit" .= trackArtistCredit track
               , "length" .= trackDuration track
               , "number" .= trackPosition track
               , "recording" .= trackRecording track
               ]
  ]

--------------------------------------------------------------------------------
testArtistRef :: Ref Editor -> EditM (Ref Artist)
testArtistRef editor = fmap coreRef . viewRevision =<< create editor
  ArtistTree
    { artistData = Artist { artistName = "Artist"
                          , artistSortName = "Artist"
                          , artistComment = ""
                          , artistBeginDate = emptyDate
                          , artistEndDate = emptyDate
                          , artistGender = Nothing
                          , artistCountry = Nothing
                          , artistType = Nothing
                          , artistEnded = False
                          }
    , artistRelationships = mempty
    , artistAliases = mempty
    , artistIpiCodes = mempty
    , artistAnnotation = mempty
    }


testArtistCredit :: Ref Editor -> EditM (Ref ArtistCredit)
testArtistCredit editor = do
  a <- testArtistRef editor
  getRef [ ArtistCreditName a "Artist" "" ]


testEditor :: MusicBrainz (Ref Editor)
testEditor = entityRef <$> register
  Editor { editorName = "ocharles", editorPassword = "" }
