{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Prelude hiding (init)
import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad (forM_)
import           Control.Monad.Trans
import           Data.Aeson (encode)
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Configurator (load, lookupDefault, Worth(..))
import           Data.Maybe (fromJust)
import           Data.Monoid (mempty)
import           Data.Text (pack)
import           Network.URI (parseURI)
import           Snap.Snaplet (runSnaplet)
import           Snap.Test hiding (buildRequest)
import           Test.Framework (buildTest, defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit (testCase)

import qualified Data.Text.Encoding as Encoding
import qualified Snap.Test as Snap
import qualified Snap.Snaplet.Test as Snaplet

import           MusicBrainz
import           MusicBrainz.Edit
import qualified MusicBrainz.Data as Data
import           MusicBrainz.Data.ArtistCredit
import qualified MusicBrainz.Data.Edit as Data
import qualified MusicBrainz.Data.Editor as Editor
import           MusicBrainz.Service (serviceInit, emptySessionStore, openSession)

import           Data.Aeson.Generic (toJSON)
import           Data.Aeson.Types (object)
import           Data.Aeson.Types (Value(..))

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain [buildTest $ fmap (testGroup "All tests") tests]
  where
    tests = do
      config <- load [Optional "autotest.cfg"]

      dbSettings <- ConnectInfo
        <$> lookupDefault (connectHost defaultConnectInfo) config "host"
        <*> lookupDefault (connectPort defaultConnectInfo) config "port"
        <*> lookupDefault "musicbrainz" config "username"
        <*> lookupDefault (connectPassword defaultConnectInfo) config "password"
        <*> lookupDefault "musicbrainz_nes" config "database"

      runMb dbSettings $
        forM_
          [ "SET client_min_messages TO warning"
          , "TRUNCATE artist_type CASCADE"
          , "TRUNCATE country CASCADE"
          , "TRUNCATE editor CASCADE"
          , "TRUNCATE gender CASCADE"
          , "ALTER SEQUENCE revision_revision_id_seq RESTART 1"
          ] $ \q -> execute q ()

      return [ testGroup "/artist"
                 [ testArtistCreate dbSettings
                 , testArtistFindLatest dbSettings
                 ]
             , testGroup "/artist-type"
                 [ testAddArtistType dbSettings
                 ]
             , testGroup "/gender"
                 [ testGenderAdd dbSettings
                 ]
             , testGroup "/label"
                 [ testLabelCreate dbSettings
                 , testLabelFindLatest dbSettings
                 ]
             , testGroup "/recording"
                 [ testRecordingFindLatest dbSettings ]
             , testGroup "/release"
                 [ testReleaseFindLatest dbSettings ]
             , testGroup "/url"
                 [ testUrlFindLatest dbSettings ]
             , testGroup "/work"
                 [ testWorkFindLatest dbSettings ]
             ]


--------------------------------------------------------------------------------
type MusicBrainzTest = ConnectInfo -> Test


--------------------------------------------------------------------------------
testArtistCreate :: MusicBrainzTest
testArtistCreate c = testCase "/create" $ testMb c $ do
    ocharles <- lift $ registerEditor
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
testArtistFindLatest :: MusicBrainzTest
testArtistFindLatest c = testCase "/find-latest" $ testMb c $ do
  artist <-  lift $ do
    ocharles <- registerEditor
    autoEdit $ Data.create (entityRef ocharles) massiveAttack >>= viewRevision
  postJson "/artist/find-latest"
    [aesonQQ|{ "mbid": <| dereference (coreRef artist) ^. re mbid |> }|]


--------------------------------------------------------------------------------
testAddArtistType :: MusicBrainzTest
testAddArtistType c = testCase "/add" $ testMb c $
  postJson "/artist-type/add" [aesonQQ|{ "name": "Person" }|]


--------------------------------------------------------------------------------
testGenderAdd :: MusicBrainzTest
testGenderAdd c = testCase "/add" $ testMb c $
  postJson "/gender/add" [aesonQQ|{ "name": "Female" }|]


--------------------------------------------------------------------------------
testLabelCreate :: MusicBrainzTest
testLabelCreate c = testCase "/create" $ testMb c $ do
  ocharles <- lift $ registerEditor
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
testLabelFindLatest :: MusicBrainzTest
testLabelFindLatest c = testCase "/find-latest" $ testMb c $ do
  label <- lift $ do
    ocharles <- registerEditor
    autoEdit $ Data.create (entityRef ocharles) labelTree >>= viewRevision

  postJson "/label/find-latest"
    [aesonQQ|{ "mbid": <| dereference (coreRef label) ^. re mbid |> }|]
  where
    labelTree = LabelTree { labelData = Label { labelName = "Warp Records"
                                              , labelSortName = "Warp Records"
                                              , labelComment = ""
                                              , labelBeginDate = emptyDate
                                              , labelEndDate = emptyDate
                                              , labelEnded = False
                                              , labelCode = Nothing
                                              , labelType = Nothing
                                              , labelCountry = Nothing
                                              }
                            , labelAliases = mempty
                            , labelIpiCodes = mempty
                            , labelAnnotation = ""
                            , labelRelationships = mempty
                            }


--------------------------------------------------------------------------------
testRecordingFindLatest :: MusicBrainzTest
testRecordingFindLatest c = testCase "/find-latest" $ testMb c $ do
  recording <- lift $ do
    ocharles <- registerEditor
    autoEdit $ do
      artist <- Data.create (entityRef ocharles) massiveAttack >>= viewRevision
      ac <- getRef [ArtistCreditName { acnArtist = coreRef artist
                                     , acnName = "Massive Attack"
                                     , acnJoinPhrase = mempty
                                     } ]
      Data.create (entityRef ocharles) (recordingTree ac) >>= viewRevision

  postJson "/recording/find-latest"
    [aesonQQ|{ "mbid": <| dereference (coreRef recording) ^. re mbid |> }|]
  where
    recordingTree ac =
      RecordingTree { recordingData = Recording { recordingName = "Warp Records"
                                                , recordingComment = ""
                                                , recordingArtistCredit = ac
                                                , recordingDuration = Nothing
                                                }
                    , recordingAnnotation = mempty
                    , recordingIsrcs = mempty
                    , recordingPuids = mempty
                    , recordingRelationships = mempty
                    }


--------------------------------------------------------------------------------
testReleaseFindLatest :: MusicBrainzTest
testReleaseFindLatest c = testCase "/find-latest" $ testMb c $ do
  release <- lift $ do
    ocharles <- registerEditor
    autoEdit $ do
      artist <- Data.create (entityRef ocharles) massiveAttack >>= viewRevision
      ac <- getRef [ArtistCreditName { acnArtist = coreRef artist
                                     , acnName = "Massive Attack"
                                     , acnJoinPhrase = mempty
                                     } ]
      rg <- Data.create (entityRef ocharles) (dummyRg ac) >>= viewRevision
      Data.create (entityRef ocharles) (releaseTree ac rg) >>= viewRevision
  postJson "/release/find-latest"
    [aesonQQ|{ "mbid": <| dereference (coreRef release) ^. re mbid |> }|]
  where
    dummyRg ac = ReleaseGroupTree { releaseGroupData = ReleaseGroup { releaseGroupName = "Dummy"
                                                                    , releaseGroupArtistCredit = ac
                                                                    , releaseGroupComment = ""
                                                                    , releaseGroupPrimaryType = Nothing
                                                                    , releaseGroupSecondaryTypes = mempty
                                                                    }
                                  , releaseGroupAnnotation = mempty
                                  , releaseGroupRelationships = mempty
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
                                          , releaseBarcode = Nothing
                                          }
                  , releaseAnnotation = mempty
                  , releaseLabels = mempty
                  , releaseMediums = mempty
                  , releaseRelationships = mempty
                  }


--------------------------------------------------------------------------------
testUrlFindLatest :: MusicBrainzTest
testUrlFindLatest c = testCase "/find-latest" $ testMb c $ do
  url <- lift $ do
    ocharles <- registerEditor
    autoEdit $ Data.create (entityRef ocharles) urlTree >>= viewRevision
  postJson "/url/find-latest"
    [aesonQQ|{ "mbid": <| dereference (coreRef url) ^. re mbid |> }|]
  where
    urlTree = UrlTree { urlData = Url { urlUrl = fromJust (parseURI "http://musicbrainz.org/") }
                      , urlRelationships = mempty
                      }


--------------------------------------------------------------------------------
testWorkFindLatest :: MusicBrainzTest
testWorkFindLatest c = testCase "/find-latest" $ testMb c $ do
  work <- lift $ do
    ocharles <- registerEditor
    autoEdit $ Data.create (entityRef ocharles) workTree >>= viewRevision
  postJson "/work/find-latest"
    [aesonQQ|{ "mbid": <| dereference (coreRef work) ^. re mbid |> }|]
  where
    workTree = WorkTree { workData = Work { workName = "To a Wild Rose"
                                          , workComment = ""
                                          , workLanguage = Nothing
                                          , workType = Nothing
                                          }
                        , workAliases = mempty
                        , workAnnotation = mempty
                        , workIswcs = mempty
                        , workRelationships = mempty
                        }


--------------------------------------------------------------------------------
postJson :: MonadIO m => BS.ByteString -> Value -> RequestBuilder m ()
postJson endPoint = postRaw endPoint "application/json" . toStrict . encode


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
                           , artistRelationships = mempty
                           , artistAliases = mempty
                           , artistIpiCodes = mempty
                           , artistAnnotation = ""
                           }

--------------------------------------------------------------------------------
registerEditor :: MusicBrainz (Entity Editor)
registerEditor = Editor.register (Editor "ocharles" "password")


--------------------------------------------------------------------------------
testMb :: ConnectInfo -> RequestBuilder MusicBrainz () -> IO ()
testMb connInfo buildRequest = do
    sessions <- emptySessionStore
    Right (token, context) <- Snaplet.evalHandler (return ()) openSession (init sessions)

    let addMbSessionHeader = addHeader "MB-Session" (Encoding.encodeUtf8 token)

    (_, service, _) <- runSnaplet (Just "autotest") (init sessions)
    runMbContext context $ do
      res <- Snap.runHandler (buildRequest >> addMbSessionHeader)
               service
      liftIO (assertSuccess res)
      rollback

  where
    init sessions = serviceInit (return connInfo) (return sessions)
