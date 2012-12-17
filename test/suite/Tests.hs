{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (forM_, void)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans
import           Data.Aeson (encode)
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromJust)
import           Data.Monoid (mempty)
import qualified Data.Set as Set
import           Data.Text
import           Network.URI (parseURI)
import           Snap.Core
import           Snap.Snaplet (runSnaplet)
import           Snap.Test hiding (buildRequest)
import           Test.Framework (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit (testCase)

import           MusicBrainz
import           MusicBrainz.Edit
import qualified MusicBrainz.Data as Data
import           MusicBrainz.Data.ArtistCredit
import qualified MusicBrainz.Data.Edit as Data
import qualified MusicBrainz.Data.Editor as Editor
import           MusicBrainz.Service (serviceInitContext)

import           Data.Aeson.Generic (toJSON)
import           Data.Aeson.Types (object)
import           Data.Aeson.Types (Value(..))

--------------------------------------------------------------------------------
main :: IO ()
main = cleanState >> defaultMain tests
  where
    tests = [ testGroup "/artist"
                [ testArtistCreate
                , testArtistFindLatest
                ]
            , testGroup "/artist-type"
                [ testAddArtistType
                ]
            , testGroup "/editor"
                [ testEditorRegister
                ]
            , testGroup "/gender"
                [ testGenderAdd
                ]
            , testGroup "/label"
                [ testLabelCreate
                , testLabelFindLatest
                ]
            , testGroup "/recording"
                [ testRecordingFindLatest ]
            , testGroup "/release"
                [ testReleaseFindLatest ]
            , testGroup "/url"
                [ testUrlFindLatest ]
            , testGroup "/work"
                [ testWorkFindLatest ]
            ]
    cleanState = runTest $ forM_
      [ "SET client_min_messages TO warning"
      , "TRUNCATE artist_type CASCADE"
      , "TRUNCATE country CASCADE"
      , "TRUNCATE editor CASCADE"
      , "TRUNCATE gender CASCADE"
      , "ALTER SEQUENCE revision_revision_id_seq RESTART 1"
      , "COMMIT"
      ] $ \q -> execute q ()


--------------------------------------------------------------------------------
testArtistCreate :: Test
testArtistCreate = testMb "/create" $
  assertApiCall buildRequest
  where
    buildRequest = do
      ocharles <- lift $ Editor.register (Editor "ocharles")
      editId <- lift $ Data.openEdit
      postJson "/artist/create" (testJson ocharles editId)
      where
        testJson editor editId = [aesonQQ| {
            "artist": {
              "name": "Massive Attack",
              "sort-name": "Massive Attack"
            },
            "editor": <| dereference $ entityRef editor |>,
            "edit": <| dereference editId |>
          } |]


--------------------------------------------------------------------------------
testArtistFindLatest :: Test
testArtistFindLatest = testMb "/find-latest" $ do
  artist <- do
    ocharles <- Editor.register (Editor "ocharles")
    autoEdit $ Data.create (entityRef ocharles) massiveAttack >>= viewRevision
  assertApiCall (buildRequest artist)
  where
    buildRequest artist = do
      postJson "/artist/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef artist) ^. remit mbid |> }|]

--------------------------------------------------------------------------------
testAddArtistType :: Test
testAddArtistType = testMb "/add" $
  assertApiCall buildRequest
  where
    buildRequest = postJson "/artist-type/add" [aesonQQ|{ "name": "Person" }|]


--------------------------------------------------------------------------------
testEditorRegister :: Test
testEditorRegister = testMb "/register" $
  assertApiCall builder
  where
    builder = postJson "/editor/register" [aesonQQ|{ "name": "ocharles" }|]


--------------------------------------------------------------------------------
testGenderAdd :: Test
testGenderAdd = testMb "/add" $
  assertApiCall buildRequest
  where
    buildRequest = postJson "/gender/add" [aesonQQ|{ "name": "Female" }|]


--------------------------------------------------------------------------------
testLabelCreate :: Test
testLabelCreate = testMb "/create" $
  assertApiCall buildRequest
  where
    buildRequest = do
      ocharles <- lift $ Editor.register (Editor "ocharles")
      editId <- lift $ Data.openEdit
      postJson "/label/create" (testJson ocharles editId)
      where
        testJson editor editId = [aesonQQ| {
            "label": {
              "name": "Warp Records",
              "sort-name": "Warp Records"
            },
            "editor": <| dereference $ entityRef editor |>,
            "edit": <| dereference editId |>
          } |]


--------------------------------------------------------------------------------
testLabelFindLatest :: Test
testLabelFindLatest = testMb "/find-latest" $ do
  label <- do
    ocharles <- Editor.register (Editor "ocharles")
    autoEdit $ Data.create (entityRef ocharles) labelTree >>= viewRevision
  assertApiCall (buildRequest label)
  where
    buildRequest label = do
      postJson "/label/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef label) ^. remit mbid |> }|]

    labelTree = LabelTree { labelData = Label { labelName = "Warp Records"
                                                  , labelSortName = "Warp Records"
                                                  , labelComment = ""
                                                  , labelBeginDate = emptyDate
                                                  , labelEndDate = emptyDate
                                                  , labelEnded = False
                                                  , labelCode = Nothing
                                                  , labelType = Nothing
                                                  }
                            , labelAliases = Set.empty
                            , labelIpiCodes = Set.empty
                            , labelAnnotation = ""
                            }


--------------------------------------------------------------------------------
testRecordingFindLatest :: Test
testRecordingFindLatest = testMb "/find-latest" $ do
  recording <- do
    ocharles <- Editor.register (Editor "ocharles")
    autoEdit $ do
      artist <- Data.create (entityRef ocharles) massiveAttack >>= viewRevision
      ac <- getRef [ArtistCreditName { acnArtist = coreRef artist
                                     , acnName = "Massive Attack"
                                     , acnJoinPhrase = mempty
                                     } ]
      Data.create (entityRef ocharles) (recordingTree ac) >>= viewRevision
  assertApiCall (buildRequest recording)
  where
    buildRequest recording = do
      postJson "/recording/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef recording) ^. remit mbid |> }|]

    recordingTree ac =
      RecordingTree { recordingData = Recording { recordingName = "Warp Records"
                                                , recordingComment = ""
                                                , recordingArtistCredit = ac
                                                , recordingDuration = Nothing
                                                }
                    , recordingAnnotation = mempty
                    , recordingIsrcs = mempty
                    , recordingPuids = mempty
                    }


--------------------------------------------------------------------------------
testReleaseFindLatest :: Test
testReleaseFindLatest = testMb "/find-latest" $ do
  release <- do
    ocharles <- Editor.register (Editor "ocharles")
    autoEdit $ do
      artist <- Data.create (entityRef ocharles) massiveAttack >>= viewRevision
      ac <- getRef [ArtistCreditName { acnArtist = coreRef artist
                                     , acnName = "Massive Attack"
                                     , acnJoinPhrase = mempty
                                     } ]
      rg <- Data.create (entityRef ocharles) (dummyRg ac) >>= viewRevision
      Data.create (entityRef ocharles) (releaseTree ac rg) >>= viewRevision
  assertApiCall (buildRequest release)
  where
    buildRequest release = do
      postJson "/release/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef release) ^. remit mbid |> }|]

    dummyRg ac = ReleaseGroupTree { releaseGroupData = ReleaseGroup { releaseGroupName = "Dummy"
                                                                    , releaseGroupArtistCredit = ac
                                                                    , releaseGroupComment = ""
                                                                    , releaseGroupPrimaryType = Nothing
                                                                    , releaseGroupSecondaryTypes = mempty
                                                                    }
                                  , releaseGroupAnnotation = mempty
                                  }

    releaseTree ac rg =
      ReleaseTree { releaseData = Release { releaseName = "Warp Records"
                                          , releaseComment = mempty
                                          , releaseArtistCredit = ac
                                          , releaseLanguage = Nothing
                                          , releaseDate = emptyDate
                                          , releaseCountry = Nothing
                                          , releaseScript = Nothing
                                          , releaseStatus = Nothing
                                          , releasePackaging = Nothing
                                          , releaseReleaseGroup = coreRef rg
                                          }
                  , releaseAnnotation = mempty
                  , releaseLabels = mempty
                  , releaseMediums = mempty
                  }


--------------------------------------------------------------------------------
testUrlFindLatest :: Test
testUrlFindLatest = testMb "/find-latest" $ do
  url <- do
    ocharles <- Editor.register (Editor "ocharles")
    autoEdit $ Data.create (entityRef ocharles) urlTree >>= viewRevision
  assertApiCall (buildRequest url)
  where
    buildRequest url = do
      postJson "/url/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef url) ^. remit mbid |> }|]

    urlTree = UrlTree { urlData = Url { urlUrl = fromJust (parseURI "http://musicbrainz.org/") } }


--------------------------------------------------------------------------------
testWorkFindLatest :: Test
testWorkFindLatest = testMb "/find-latest" $ do
  work <- do
    ocharles <- Editor.register (Editor "ocharles")
    autoEdit $ Data.create (entityRef ocharles) workTree >>= viewRevision
  assertApiCall (buildRequest work)
  where
    buildRequest work = do
      postJson "/work/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef work) ^. remit mbid |> }|]

    workTree = WorkTree { workData = Work { workName = "To a Wild Rose"
                                          , workComment = ""
                                          , workLanguage = Nothing
                                          , workType = Nothing
                                          }
                        , workAliases = mempty
                        , workAnnotation = mempty
                        , workIswcs = mempty
                        }


--------------------------------------------------------------------------------
postJson :: MonadIO m => BS.ByteString -> Value -> RequestBuilder m ()
postJson endPoint = postRaw endPoint "application/json" . toStrict . encode


--------------------------------------------------------------------------------
testMb :: String -> MusicBrainz a -> Test
testMb label action = testCase label . runTest . void $ action


--------------------------------------------------------------------------------
assertApiCall :: RequestBuilder MusicBrainz () -> MusicBrainz Response
assertApiCall buildRequest = do
  ctx <- ask
  (_, snap, _) <- liftIO $ runSnaplet (Just "autotest") (serviceInitContext (return ctx))
  r <- runHandler buildRequest snap
  liftIO $ assertSuccess r
  return r


--------------------------------------------------------------------------------
runTest :: MusicBrainz a -> IO a
runTest = runMb databaseSettings . withTransactionRollBack
  where databaseSettings = defaultConnectInfo { connectDatabase = "musicbrainz_nes_service"
                                              , connectUser = "musicbrainz"
                                              }


--------------------------------------------------------------------------------
toStrict :: LBS.ByteString -> BS.ByteString
toStrict = BS.concat . LBS.toChunks


--------------------------------------------------------------------------------
autoEdit :: EditM a -> MusicBrainz a
autoEdit action = do
  editId <- Data.openEdit
  Data.withEdit editId action <* Data.apply editId


--------------------------------------------------------------------------------
massiveAttack :: Tree Artist
massiveAttack = ArtistTree { artistData = Artist { artistName = "Massive Attack"
                                                 , artistSortName = "Massive Attack"
                                                 , artistComment = ""
                                                 , artistBeginDate = emptyDate
                                                 , artistEndDate = emptyDate
                                                 , artistEnded = False
                                                 , artistGender = Nothing
                                                 , artistType = Nothing
                                                 , artistCountry = Nothing
                                                 }
                           , artistRelationships = Set.empty
                           , artistAliases = Set.empty
                           , artistIpiCodes = Set.empty
                           , artistAnnotation = ""
                           }
