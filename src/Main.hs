{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Exception (SomeException, try)
import           Control.Lens hiding (Context, view)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Aeson (encode, ToJSON, json)
import           Data.Attoparsec.Lazy (parse, maybeResult)
import           Data.Configurator (lookupDefault)
import           Data.Map (Map)
import           Data.Text (Text)
import           Snap (SnapletInit, makeSnaplet, serveSnaplet, defaultConfig, Handler, getSnapletUserConfig, addRoutes)
import           Snap.Core (writeLBS, setContentType, modifyResponse, setResponseCode, readRequestBody)
import           Text.Digestive (Form)
import           Text.Digestive.Aeson (digestJSON)
import           Text.Digestive.View (View, viewErrors)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified MusicBrainz.API.Artist as Artist
import qualified MusicBrainz.API.Label as Label
import qualified MusicBrainz.API.ReleaseGroup as ReleaseGroup

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, connectPassword, MusicBrainz, Context, openContext, runMbContext, withTransaction)
import           MusicBrainz.API.JSON ()

--------------------------------------------------------------------------------
expose :: ToJSON a => Form Text MusicBrainz a -> Handler Service Service ()
expose f = do
  parsedJson <- maybeResult . parse json <$> readRequestBody (1024*1024)

  case parsedJson of
    Nothing -> do
      -- The JSON the client submitted could not be parsed, so fail
      modifyResponse (setResponseCode 400)

    Just json' -> do
      -- We run the 'form' that validates the users submitted parameters to the
      -- API call.

      -- We run the 'form' that validates the users submitted parameters to the API
      -- call.
      context <- gets svcContext
      outcome <- liftIO (try (runMbContext context (withTransaction $ digestJSON f json')))
      modifyResponse (setContentType "application/json")
      case outcome of
        Left (exception :: SomeException) -> do
          -- There was indeed an exception, so lets render that back to the
          -- client.
          modifyResponse (setResponseCode 500)
          writeLBS . encode $ Map.fromList [("error"::Text, show exception)]

        Right (view, ret') ->
          case ret' of
            Just success ->
              -- All went smoothly, so just render back the API result.
              writeLBS (encode success)

            Nothing -> do
              -- The client hasn't submitted valid parameters, so we'll render back
              -- a list of all the parameters that failed validation.
              modifyResponse (setResponseCode 400)
              writeLBS (encode $ errorMap view)


--------------------------------------------------------------------------------
errorMap :: View Text -> Map Text Text
errorMap = Map.fromList . over (mapped._1) (T.intercalate ".") . viewErrors


--------------------------------------------------------------------------------
data Service = Service { svcContext :: Context }

serviceInit :: SnapletInit Service Service
serviceInit = makeSnaplet "service" "musicbrainz-data HTTP service" Nothing $ do
  config <- getSnapletUserConfig
  [db, user, pass] <- liftIO $ forM
    [ ("database", connectDatabase)
    , ("username", connectUser)
    , ("password", connectPassword)
    ] $ \(key, def) -> lookupDefault (def defaultConnectInfo) config key
  addRoutes
    [ ("/artist/find-latest", expose Artist.findLatest)
    , ("/artist/create", expose Artist.create)

    , ("/label/create", expose Label.create)
    , ("/label/find-latest", expose Label.findLatest)

    , ("/release-group/create", expose ReleaseGroup.create)
    ]

  Service <$> liftIO (openContext defaultConnectInfo
    { connectDatabase = db
    , connectUser = user
    , connectPassword = pass
    })


--------------------------------------------------------------------------------
main :: IO ()
main = serveSnaplet defaultConfig serviceInit
