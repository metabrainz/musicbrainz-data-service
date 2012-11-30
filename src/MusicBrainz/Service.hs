{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Service (serviceInit, serviceInitContext) where

import           Control.Applicative
import           Control.Exception (SomeException, try)
import           Control.Lens hiding (Context, view)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Aeson (decode, encode, object, ToJSON(toJSON), Value)
import           Data.Aeson.Lens
import           Data.Configurator (lookupDefault)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Safe
import           Snap (Initializer, SnapletInit, makeSnaplet, Handler, getSnapletUserConfig, addRoutes)
import           Snap.Core (writeLBS, setContentType, modifyResponse, setResponseCode, readRequestBody)
import           Text.Digestive (Form)
import           Text.Digestive.Aeson (digestJSON)
import           Text.Digestive.View (View, viewErrors)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified MusicBrainz.API.Artist as Artist
import qualified MusicBrainz.API.ArtistType as ArtistType
import qualified MusicBrainz.API.Editor as Editor
import qualified MusicBrainz.API.Gender as Gender
import qualified MusicBrainz.API.Label as Label
import qualified MusicBrainz.API.ReleaseGroup as ReleaseGroup

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, connectPassword, MusicBrainz, Context, openContext, runMbContext, withTransaction)
import           MusicBrainz.API.JSON ()

--------------------------------------------------------------------------------
expose :: ToJSON a => Form Text MusicBrainz a -> Handler Service Service ()
expose f = do
  parsedJson <- decode <$> readRequestBody (1024*1024)

  case (parsedJson :: Maybe Value) of
    Nothing -> do
      -- The JSON the client submitted could not be parsed, so fail
      modifyResponse (setResponseCode 400)
      writeLBS . encode $ Map.fromList [("error"::Text, "Could not parse JSON"::Text)]

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
errorMap :: View Text -> Value
errorMap = fromMaybe (error "Constructing error tree failed!") .
    foldl encodeError (Just $ object []) . viewErrors
  where
    encodeError json (path, message) = json & pathToLens path .~ Just (toJSON message)
    pathToLens = foldl (.) id . map pathElem
    pathElem p = maybe (key p) nth (readMay $ T.unpack p)


--------------------------------------------------------------------------------
data Service = Service { svcContext :: Context }

serviceInit :: SnapletInit Service Service
serviceInit = serviceInitContext $ do
  config <- getSnapletUserConfig
  [db, user, pass] <- liftIO $ forM
    [ ("database", connectDatabase)
    , ("username", connectUser)
    , ("password", connectPassword)
    ] $ \(k, def) -> lookupDefault (def defaultConnectInfo) config k
  liftIO (openContext defaultConnectInfo
    { connectDatabase = db
    , connectUser = user
    , connectPassword = pass
    })

serviceInitContext :: Initializer Service Service Context -> SnapletInit Service Service
serviceInitContext ctxInit = makeSnaplet "service" "musicbrainz-data HTTP service" Nothing $ do
  addRoutes
    [ ("/artist/create", expose Artist.create)
    , ("/artist/find-latest", expose Artist.findLatest)

    , ("/artist-type/add", expose ArtistType.add)

    , ("/editor/register", expose Editor.register)

    , ("/gender/add", expose Gender.add)

    , ("/label/create", expose Label.create)
    , ("/label/find-latest", expose Label.findLatest)

    , ("/release-group/create", expose ReleaseGroup.create)
    ]

  Service <$> ctxInit
