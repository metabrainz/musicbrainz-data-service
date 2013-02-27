{-# LANGUAGE OverloadedStrings #-}
module Parsers (tests) where

import Control.Applicative
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Text.Lens
import MusicBrainz
import Network.URI (parseURI)
import Text.Digestive

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.ArtistCredit as MB
import qualified MusicBrainz.Data.Edit as MB
import qualified MusicBrainz.Data.Editor as MB
import qualified MusicBrainz.API as API

import Test.MusicBrainz hiding (expectFailure)

import Common

--------------------------------------------------------------------------------
tests :: [Test]
tests =
  [ testAnnotation
  , testMbid
  , testNonEmptyText
  , testArtistCreditRef
  , testArtistTypeRef
  , testWorkTypeRef
  , testCountryRef
  , testEditorRef
  , testGenderRef
  , testLabelTypeRef
  , testLanguageRef
  , testReleaseGroupTypeRef
  , testArtist
  , testLabel
  , testReleaseGroup
  , testUrl
  , testEdit
  , testRevision
  , testAliases
  , testRelationships
  , testOptionalCoreRef
  , testDuration
  , testIpiCodes
  , testWork
  ]



--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "annotation" $
   testForm API.annotation
     [ (["annotation"], expected) ]
     expected
 where expected = "My annotation here"



--------------------------------------------------------------------------------
testMbid :: Test
testMbid = testGroup "mbid"
  [ testCase "Successful parse" $
      testMbid' "781db52b-8d7f-49cb-9df0-d4289b26e75d"
  , testCase "Invalid parse" $
      testMbid' "I like turtles"
  ]
  where
    testMbid' expected = do
      (_, actual) <- mockPost API.mbid [(["mbid"], expected)]
      actual @?= (expected ^? unpacked . mbid)


--------------------------------------------------------------------------------
testNonEmptyText :: Test
testNonEmptyText = testGroup "nonEmptyText"
  [ testCase "Successful parse" $ do
      let expected = "I like turtles"
      testForm
        ("t" .: API.nonEmptyText)
        [(["t"], expected)]
        expected
  , testCase "Invalid parse" $
      expectFailure ("t" .: API.nonEmptyText) [(["t"], "")]
  ]


--------------------------------------------------------------------------------
testArtistCreditRef :: Test
testArtistCreditRef = testGroup "artistCreditRef"
    [ testCase "No artists is a failure" $ go []
    , testCase "Successful with one artist" $ do
        editor <- registerEditor
        boc <- mkArtist editor "Boards of Canada"
        go [ArtistCreditName boc "A name" ""]
    , testCase "Successful with more than one artist" $ do
        editor <- registerEditor
        mefjus <- mkArtist editor "Mefjus"
        mforce <- mkArtist editor "M-Force"
        go [ ArtistCreditName mefjus "Mefjus" " & "
           , ArtistCreditName mforce "M-Force" ""
           ]
    ]
  where
    go names
      | null names = expectFailure API.artistCreditRef (mockAcFields names)
      | otherwise  = do
          acId <- success API.artistCreditRef (mockAcFields names)
          actual <- expandCredit acId
          actual @?= names


--------------------------------------------------------------------------------
testArtistTypeRef :: Test
testArtistTypeRef = testCase "artistTypeRef" $
  testOptionalAddParser
    ("type" .: API.artistTypeRef)
    "type"
    (ArtistType { artistTypeName = "Person" })


--------------------------------------------------------------------------------
testWorkTypeRef :: Test
testWorkTypeRef = testCase "workTypeRef" $
  testOptionalAddParser
    ("type" .: API.workTypeRef)
    "type"
    (WorkType { workTypeName = "Person" })


--------------------------------------------------------------------------------
testCountryRef :: Test
testCountryRef = testCase "countryTypeRef" $
  testOptionalAddParser
    ("type" .: API.countryRef)
    "type"
    (Country { countryIsoCode = "UK", countryName = "UK" })


--------------------------------------------------------------------------------
testEditorRef :: Test
testEditorRef = testCase "editorRef" $ do
  ref <- entityRef <$> registerEditor
  testForm
    API.editor
    [(["editor"], ref ^. to dereference . to show . packed)]
    ref


--------------------------------------------------------------------------------
testEdit :: Test
testEdit = testCase "edit" $ do
  ref <- MB.openEdit
  testForm
    API.edit
    [(["edit"], ref ^. to dereference . to show . packed)]
    ref


--------------------------------------------------------------------------------
testRevision :: Test
testRevision = testCase "revision" $ do
  editor <- registerEditor
  w <- autoEdit $ MB.create (entityRef editor)
    WorkTree { workRelationships = mempty
             , workAliases = mempty
             , workAnnotation = ""
             , workIswcs = mempty
             ,  workData =
                 Work { workName = "Katana"
                      , workLanguage = Nothing
                      , workType = Nothing
                      , workComment = ""
                      }
             }

  testForm
    API.revision
    [(["revision"], w ^. to dereference . to show . packed)]
    w


--------------------------------------------------------------------------------
testGenderRef :: Test
testGenderRef = testCase "genderTypeRef" $
  testOptionalAddParser
    ("type" .: API.genderRef)
    "type"
    (Gender { genderName = "Male" })


--------------------------------------------------------------------------------
testLabelTypeRef :: Test
testLabelTypeRef = testCase "labelTypeTypeRef" $
  testOptionalAddParser
    ("type" .: API.labelTypeRef)
    "type"
    (LabelType { labelTypeName = "Original Production" })


--------------------------------------------------------------------------------
testLanguageRef :: Test
testLanguageRef = testCase "languageTypeRef" $
  testOptionalAddParser
    ("type" .: API.languageRef)
    "type"
    (Language { languageName = "English"
              , languageIsoCode2t = "en"
              , languageIsoCode2b = "en"
              , languageIsoCode1 = "en"
              , languageIsoCode3 = "eng"
              })


--------------------------------------------------------------------------------
testReleaseGroupTypeRef :: Test
testReleaseGroupTypeRef = testGroup "releaseGroupTypeRef"
  [ testCase "Primary" $
      testOptionalAddParser
        ("type" .: API.releaseGroupTypeRef)
        "type"
        (ReleaseGroupType { releaseGroupTypeName = "Single" } :: ReleaseGroupType Primary)
  , testCase "Secondary" $
      testOptionalAddParser
        ("type" .: API.releaseGroupTypeRef)
        "type"
        (ReleaseGroupType { releaseGroupTypeName = "Remix" } :: ReleaseGroupType Secondary)
  ]


--------------------------------------------------------------------------------
testArtist :: Test
testArtist = testCase "artist" $
  let expected = Artist { artistName = "DSP"
                        , artistSortName = "DSP"
                        , artistComment = ""
                        , artistBeginDate = emptyDate
                        , artistEndDate = emptyDate
                        , artistGender = Nothing
                        , artistType = Nothing
                        , artistCountry = Nothing
                        , artistEnded = False
                        }
  in testForm
    API.artist
    [ (["name"], artistName expected)
    , (["sort-name"], artistSortName expected)
    ]
    expected


--------------------------------------------------------------------------------
testLabel :: Test
testLabel = testCase "label" $
  let expected = Label { labelName = "DSP"
                       , labelSortName = "DSP"
                       , labelComment = ""
                       , labelBeginDate = emptyDate
                       , labelEndDate = emptyDate
                       , labelType = Nothing
                       , labelCountry = Nothing
                       , labelEnded = False
                       , labelCode = Just 12345
                       }
  in testForm
    API.label
    [ (["name"], labelName expected)
    , (["sort-name"], labelSortName expected)
    , (["code"], maybe mempty (view packed . show) $ labelCode expected)
    ]
    expected


--------------------------------------------------------------------------------
testReleaseGroup :: Test
testReleaseGroup = testCase "releaseGroup" $ do
  editor <- registerEditor
  earlGrey <- mkArtist editor "Earl Grey"

  let name = "Subtle Audio, Volume II"
  let comment = "Drum and bass"
  let names = [ ArtistCreditName earlGrey "Earl Grey" "" ]
  let primaryType = Nothing
  let secondaryTypes = mempty

  rg <- success
    API.releaseGroup
    ([ (["name"], name)
     , (["comment"], comment)
     ] ++ mockAcFields names)

  releaseGroupName rg @?= name
  releaseGroupComment rg @?= comment
  releaseGroupPrimaryType rg @?= primaryType
  releaseGroupSecondaryTypes rg @?= secondaryTypes

  credits <- expandCredit (releaseGroupArtistCredit rg)
  credits @?= names


--------------------------------------------------------------------------------
testUrl :: Test
testUrl = testCase "url" $ do
  let expected = Url { urlUrl = fromMaybe (error "Failed to parse URL") $
                                  parseURI "http://musicbrainz.org/" }
  testForm API.url [([], show (urlUrl expected) ^. packed)] expected


--------------------------------------------------------------------------------
testWork :: Test
testWork = testCase "work" $ do
  let expected = Work { workName = "From a Bar in an Airport"
                      , workComment = ""
                      , workLanguage = Nothing
                      , workType = Nothing
                      }
  testForm API.work [ (["name"], workName expected)
                    , (["comment"], workComment expected)
                    ] expected

--------------------------------------------------------------------------------
testOptionalAddParser :: (MB.Add a, Referenceable a)
                      => Form Text MusicBrainz (Maybe (Ref a))
                      -> Text
                      -> a
                      -> MusicBrainz ()
testOptionalAddParser form fieldName x = do
  ref <- entityRef <$> MB.add x
  testForm
    form
    [([fieldName], ref ^. to dereference . to show . packed)]
    (Just ref)


--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "aliases" $
    testForm API.aliases fields (Set.fromList expected)
  where
    expected =
      [ Alias { aliasName = "RATM"
              , aliasSortName = "RATM"
              , aliasBeginDate = emptyDate
              , aliasEndDate = emptyDate
              , aliasType = Nothing
              , aliasLocale = Nothing
              , aliasPrimaryForLocale = False
              , aliasEnded = False
              } :: Alias Artist
      ]
    fields = concat
      [ [ (["alias", "indices"], formatIndices expected) ]
      , mkFields aliasField expected
      ]
    aliasField i alias =
      [ (["aliases", i, "name"], aliasName alias)
      , (["aliases", i, "sort-name"], aliasSortName alias)
      ]


--------------------------------------------------------------------------------
testRelationships :: Test
testRelationships = testCase "relationships" $ do
    expected <- mkExpected
    testForm API.relationships (fields expected) (Set.fromList expected)
  where
    fields expected =
      concat [ [ (["relationships", "work", "indices"], formatIndices expected) ]
             , map (\t -> (["relationships", t, "indices"], ""))
                 [ "artist", "label", "release", "recording", "release-group", "url" ]
             , mkFields relationshipField expected
             ]

    relationshipField i (WorkRelationship target rel) =
      [ (["relationships", "work", i, "target"], target ^. to dereference . re mbid . packed)
      , (["relationships", "work", i, "type"], relType rel ^. to dereference . to show . packed)
      , (["relationships", "work", i, "attributes", "indices"], formatIndices $ Set.toList $ relAttributes rel)
      ]

    mkWork editor name = fmap coreRef $
      MB.create (entityRef editor)
        (WorkTree { workRelationships = mempty
                  , workAliases = mempty
                  , workAnnotation = ""
                  , workIswcs = mempty
                  ,  workData =
                      Work { workName = name
                           , workLanguage = Nothing
                           , workType = Nothing
                           , workComment = ""
                           }
                  }) >>= MB.viewRevision

    mkExpected = do
      editor <- registerEditor
      (katana, brother) <- autoEdit $
        (,) <$> mkWork editor "Katana"
            <*> mkWork editor "Brother: The Point (2562 remix)"

      relType <- entityRef <$> MB.add
        RelationshipType { relName = "R"
                         , relTypeAttributes = mempty
                         , relParent = Nothing
                         , relChildOrder = 1
                         , relLeftTarget = ToArtist
                         , relRightTarget = ToWork
                         , relDescription = ""
                         , relLinkPhrase = ""
                         , relReverseLinkPhrase = ""
                         , relShortLinkPhrase = ""
                         , relPriority = 0
                         }

      return
        [ WorkRelationship brother
            Relationship { relType = relType
                         , relBeginDate = emptyDate
                         , relAttributes = mempty
                         , relEndDate = emptyDate
                         , relEnded = False
                         }
        , WorkRelationship katana
            Relationship { relType = relType
                         , relBeginDate = emptyDate
                         , relAttributes = mempty
                         , relEndDate = emptyDate
                         , relEnded = False
                         }
        ]


--------------------------------------------------------------------------------
testOptionalCoreRef :: Test
testOptionalCoreRef = testGroup "optionalCoreRef"
    [ testCase "Missing" $ testForm form [] Nothing
    , testCase "Present & invalid" $
        expectFailure form [ ([], "Mmm, waffles") ]
    ]
  where
    form :: Form Text MusicBrainz (Maybe (Ref Work))
    form = API.optionalCoreRef


--------------------------------------------------------------------------------
testDuration :: Test
testDuration = testGroup "duration"
  [ testCase "Valid" $
      testForm API.duration [(["duration"], "74947")] (Just 74947)
  , testCase "Missing" $
      testForm API.duration [] Nothing
  , testCase "Invalid" $
      expectFailure API.duration [(["duration"], "One minute and ten seconds")]
  ]


--------------------------------------------------------------------------------
testIpiCodes :: Test
testIpiCodes = testGroup "ipiCodes"
  [ testCase "None" $ testForm API.ipiCodes [(["ipi-codes", "indices"], "")] mempty
  , testCase "One" $ do
      let expected = "00244121213" ^?! ipi
      testForm
        API.ipiCodes
        [ (["ipi-codes", "indices"], "0")
        , (["ipi-codes", "0"], expected ^. re ipi)
        ]
        (Set.singleton expected)
  , testCase "Invalid" $
      expectFailure API.ipiCodes
        [ (["ipi-codes", "indices"], "0")
        , (["ipi-codes", "0"], "Madonna")
        ]
  ]


--------------------------------------------------------------------------------
expandCredit :: Ref ArtistCredit -> MusicBrainz [ArtistCreditName]
expandCredit acId = (Map.! acId) <$> MB.expandCredits (Set.singleton acId)
