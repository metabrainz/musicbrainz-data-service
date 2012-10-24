{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Exception (SomeException, try)
import           Control.Lens hiding (view)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Aeson (encode, ToJSON)
import           Data.Configurator (require)
import           Data.Map (Map)
import           Data.Text (Text)
import           Snap (SnapletInit, makeSnaplet, serveSnaplet, defaultConfig, Handler, getSnapletUserConfig, addRoutes)
import           Snap.Core (writeLBS, setContentType, modifyResponse, setResponseCode)
import           Text.Digestive (Method(Post), Form)
import           Text.Digestive.Snap (method, defaultSnapFormConfig, runFormWith)
import           Text.Digestive.View (View, viewErrors)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified MusicBrainz.API.Artist as Artist
import qualified MusicBrainz.API.Label as Label
import qualified MusicBrainz.API.ReleaseGroup as ReleaseGroup

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, connectPassword, MusicBrainz, Context, openContext, runMbContext)
import           MusicBrainz.API.JSON ()

expose :: ToJSON a => Form Text (Handler Service Service) (MusicBrainz a) -> Handler Service Service ()
expose f = do
  -- We run the 'form' that validates the users submitted parameters to the API
  -- call.
  (view, ret) <- postForm "api" f
  case ret of
    Just r -> do
      -- The parameters validate to a valid API call, so we make it.
      -- There's still a risk of explosions which we want to convey correctly,
      -- so we 'try' to run the action.
      context <- gets svcContext
      outcome <- liftIO (try (runMbContext context r))

      modifyResponse (setContentType "application/json")
      case outcome of
        Left (exception :: SomeException) -> do
          -- There was indeed an exception, so lets render that back to the
          -- client.
          modifyResponse (setResponseCode 500)
          writeLBS . encode $ Map.fromList [("error"::Text, show exception)]
        Right success ->
          -- All went smoothly, so just render back the API result.
          writeLBS (encode success)

    Nothing -> do
      -- The client hasn't submitted valid parameters, so we'll render back
      -- a list of all the parameters that failed validation.
      modifyResponse (setResponseCode 400)
      writeLBS (encode $ errorMap view)

  where
    postForm = runFormWith defaultSnapFormConfig { method = Just Post }

errorMap :: View Text -> Map Text Text
errorMap = Map.fromList . over (mapped._1) (T.intercalate ".") . viewErrors


--------------------------------------------------------------------------------
data Service = Service { svcContext :: Context }

serviceInit :: SnapletInit Service Service
serviceInit = makeSnaplet "service" "musicbrainz-data HTTP service" Nothing $ do
  config <- getSnapletUserConfig
  db <- liftIO $ require config "database"
  user <- liftIO $ require config "username"
  password <- liftIO $ require config "password"
  addRoutes
    [("/artist/find-latest", expose Artist.findLatest)
    ,("/artist/create", expose Artist.create)

    ,("/label/find-latest", expose Label.findLatest)

    ,("/release-group/find-latest", expose ReleaseGroup.findLatest)
    ,("/release-group/create", expose ReleaseGroup.create)
    ]

  Service <$> liftIO (openContext defaultConnectInfo
    { connectDatabase = db
    , connectUser = user
    , connectPassword = password
    })

main :: IO ()
main = serveSnaplet defaultConfig serviceInit
