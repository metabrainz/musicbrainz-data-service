{-# LANGUAGE OverloadedStrings #-}
module JSON (tests) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Monoid (mempty)
import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.ArtistCredit
import MusicBrainz.Data.Editor
import Test.MusicBrainz

import MusicBrainz.API.JSON ()

tests :: [Test]
tests =
  [ testGroup "Release"
      [ testCase "Full release" $ do
          editor <- entityRef <$> register Editor { editorName = "ocharles", editorPassword = "" }
          release <- autoEdit $ do
            a <- fmap coreRef . viewRevision =<< create editor
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

            ac <- getRef [ ArtistCreditName a "Artist" "" ]

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

            viewRevision =<< create editor
              ReleaseTree
                { releaseData = Release { releaseName = "Release"
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
                , releaseAnnotation = mempty
                , releaseLabels = mempty
                , releaseMediums = mempty
                , releaseRelationships = mempty
                }

          let r = coreData release
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
  ]
