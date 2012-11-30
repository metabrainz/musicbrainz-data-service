{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (forM_, void)
import           Control.Monad.CatchIO
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans
import           Data.Aeson (decode, encode)
import           Data.Aeson.Lens
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import           Data.Text
import           Snap.Core
import           Snap.Snaplet (runSnaplet)
import           Snap.Test hiding (buildRequest)
import           Test.Framework (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit hiding (Label, Test, assert)

import           MusicBrainz
import qualified MusicBrainz.Data as Data
import qualified MusicBrainz.Data.Editor as Editor
import           MusicBrainz.Service (serviceInitContext)

import           Data.Aeson.Generic (toJSON)
import           Data.Aeson.Types (object)
import           Data.Aeson.Types (Value(..))

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
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
            ]


--------------------------------------------------------------------------------
testArtistCreate :: Test
testArtistCreate = testMb "/create" $
  assertApiCall buildRequest assert
  where
    buildRequest = do
      ocharles <- lift $ Editor.register (Editor "ocharles")
      postJson "/artist/create" (testJson ocharles)
      where
        testJson editor = [aesonQQ| {
            "artist": {
              "name": "Massive Attack",
              "sort-name": "Massive Attack"
            },
            "editor": <| dereference $ entityRef editor |>
          } |]

    assert actual = do
      liftIO $ actual @?= expected
      where
        expected = Just [aesonQQ| {
            "mbid": null,
            "data": {
              "name": "Massive Attack",
              "sort-name": "Massive Attack",
              "begin-date": {"year": null, "day": null, "month": null},
              "end-date": {"year": null, "day": null, "month": null},
              "ended": false,
              "type": null,
              "country": null,
              "comment": "",
              "gender": null
            }
          } |] & key "mbid" .~ (actual ^. key "mbid" :: Maybe String)


--------------------------------------------------------------------------------
testArtistFindLatest :: Test
testArtistFindLatest = testMb "/find-latest" $ do
  artist <- do
    ocharles <- Editor.register (Editor "ocharles")
    Data.create (entityRef ocharles) artistTree
  assertApiCall (buildRequest artist) (assert artist)
  where
    buildRequest artist = do
      postJson "/artist/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef artist) ^. by mbid |> }|]

    artistTree = ArtistTree { artistData = Artist { artistName = "Massive Attack"
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

    assert artist res =
      liftIO $ res @?= expected
      where
        expected = Just [aesonQQ| {
            "mbid": <| dereference (coreRef artist) ^. by mbid |>,
            "data": {
              "name": "Massive Attack",
              "sort-name": "Massive Attack",
              "begin-date": {"year": null, "day": null, "month": null},
              "end-date": {"year": null, "day": null, "month": null},
              "ended": false,
              "type": null,
              "country": null,
              "comment": "",
              "gender": null
            }
          } |]


--------------------------------------------------------------------------------
testAddArtistType :: Test
testAddArtistType = testMb "/add" $
  assertApiCall buildRequest assert
  where
    buildRequest = postJson "/artist-type/add" [aesonQQ|{ "name": "Person" }|]
    assert res =
      liftIO $ res @?= expected
      where
        expected =
          Just [aesonQQ| { "data": {"name": "Person"} } |]
            & key "ref" .~ (res ^. key "ref" :: Maybe Int)


--------------------------------------------------------------------------------
testEditorRegister :: Test
testEditorRegister = testMb "/register" $
  assertApiCall builder assert
  where
    builder = postJson "/editor/register" [aesonQQ|{ "name": "ocharles" }|]
    assert res = liftIO $ res @?= expected
      where
        expected = Just [aesonQQ| {
          "data": {"name": "ocharles"}
        } |] & key "ref" .~ (res ^. key "ref" :: Maybe Int)


--------------------------------------------------------------------------------
testGenderAdd :: Test
testGenderAdd = testMb "/add" $
  assertApiCall buildRequest assert
  where
    buildRequest = postJson "/gender/add" [aesonQQ|{ "name": "Female" }|]
    assert res =
      liftIO $ res @?= expected
      where
        expected =
          Just [aesonQQ| { "data": {"name": "Female"} } |]
            & key "ref" .~ (res ^. key "ref" :: Maybe Int)


--------------------------------------------------------------------------------
testLabelCreate :: Test
testLabelCreate = testMb "/create" $
  assertApiCall buildRequest assert
  where
    buildRequest = do
      ocharles <- lift $ Editor.register (Editor "ocharles")
      postJson "/label/create" (testJson ocharles)
      where
        testJson editor = [aesonQQ| {
            "label": {
              "name": "Warp Records",
              "sort-name": "Warp Records"
            },
            "editor": <| dereference $ entityRef editor |>
          } |]

    assert actual = do
      liftIO $ actual @?= expected
      where
        expected = Just [aesonQQ| {
            "mbid": null,
            "data": {
              "name": "Warp Records",
              "sort-name": "Warp Records",
              "begin-date": {"year": null, "day": null, "month": null},
              "end-date": {"year": null, "day": null, "month": null},
              "ended": false,
              "type": null,
              "comment": "",
              "label-code": null
            }
          } |] & key "mbid" .~ (actual ^. key "mbid" :: Maybe String)


--------------------------------------------------------------------------------
testLabelFindLatest :: Test
testLabelFindLatest = testMb "/find-latest" $ do
  label <- do
    ocharles <- Editor.register (Editor "ocharles")
    Data.create (entityRef ocharles) labelTree
  assertApiCall (buildRequest label) (assert label)
  where
    buildRequest label = do
      postJson "/label/find-latest"
        [aesonQQ|{ "mbid": <| dereference (coreRef label) ^. by mbid |> }|]

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

    assert label res =
      liftIO $ res @?= expected
      where
        expected = Just [aesonQQ| {
            "mbid": <| dereference (coreRef label) ^. by mbid |>,
            "data": {
              "name": "Warp Records",
              "sort-name": "Warp Records",
              "begin-date": {"year": null, "day": null, "month": null},
              "end-date": {"year": null, "day": null, "month": null},
              "ended": false,
              "type": null,
              "comment": "",
              "label-code": null
            }
          } |]


--------------------------------------------------------------------------------
postJson :: MonadIO m => BS.ByteString -> Value -> RequestBuilder m ()
postJson endPoint = postRaw endPoint "application/json" . toStrict . encode


--------------------------------------------------------------------------------
apiCall :: RequestBuilder MusicBrainz () -> MusicBrainz Response
apiCall rb = do
  ctx <- ask
  (_, snap, _) <- liftIO $ runSnaplet (Just "autotest") (serviceInitContext (return ctx))
  r <- runHandler rb snap
  liftIO $ assertSuccess r
  return r


--------------------------------------------------------------------------------
testMb :: String -> MusicBrainz a -> Test
testMb label action = testCase label . runTest . void $ cleanState >> action
  where
    cleanState = forM_
      [ "SET client_min_messages TO warning"
      , "TRUNCATE artist_type CASCADE"
      , "TRUNCATE country CASCADE"
      , "TRUNCATE editor CASCADE"
      , "TRUNCATE gender CASCADE"
      , "ALTER SEQUENCE revision_revision_id_seq RESTART 1"
      ] $ \q -> execute q ()


--------------------------------------------------------------------------------
assertApiCall :: RequestBuilder MusicBrainz () -> (Maybe Value -> MusicBrainz a) -> MusicBrainz a
assertApiCall buildRequest verifyJson = apiCall buildRequest >>= verify
  where
    verify r = liftIO (getResponseBody r) >>= \body -> verifyJson $ decode (fromStrict body)


--------------------------------------------------------------------------------
runTest :: MusicBrainz a -> IO a
runTest a = runMb databaseSettings $
  begin *> a `onException` rollback <* rollback
  where databaseSettings = defaultConnectInfo { connectDatabase = "musicbrainz_nes"
                                              , connectUser = "musicbrainz"
                                              }


--------------------------------------------------------------------------------
fromStrict :: BS.ByteString -> LBS.ByteString
fromStrict x = LBS.fromChunks [x]

toStrict :: LBS.ByteString -> BS.ByteString
toStrict = BS.concat . LBS.toChunks
