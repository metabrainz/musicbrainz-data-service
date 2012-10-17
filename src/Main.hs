{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception (SomeException, try)
import           Control.Lens hiding (view)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (encode, ToJSON)
import           Data.Map (Map)
import           Data.Text (Text)
import           Snap.Core (route, writeLBS, setContentType, modifyResponse, setResponseCode, MonadSnap)
import           Snap.Http.Server (quickHttpServe)
import           Text.Digestive (Method(Post), Form)
import           Text.Digestive.Snap (method, defaultSnapFormConfig, runFormWith)
import           Text.Digestive.View (View, viewErrors)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified MusicBrainz.API.Artist as Artist
import qualified MusicBrainz.API.Label as Label

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, runMb, MusicBrainz)
import           MusicBrainz.API.JSON ()

expose :: (MonadSnap m, ToJSON a) => Form Text m (MusicBrainz a) -> m ()
expose f = do
  (view, ret) <- runFormWith defaultSnapFormConfig { method = Just Post } "api" f
  case ret of
    Just r -> do
      outcome <- liftIO (try (runMbAction r))

      modifyResponse (setContentType "application/json")
      case outcome of
        Left (exception :: SomeException) -> do
          modifyResponse (setResponseCode 500)
          writeLBS . encode $ Map.fromList [("error"::Text, show exception)]
        Right success -> writeLBS (encode success)

    Nothing -> do
      modifyResponse (setResponseCode 400)
      writeLBS (encode $ errorMap view)

errorMap :: View Text -> Map Text Text
errorMap = Map.fromList . over (mapped._1) (T.intercalate ".") . viewErrors

runMbAction :: MusicBrainz a -> IO a
runMbAction = runMb defaultConnectInfo { connectDatabase = "musicbrainz_nes", connectUser = "musicbrainz" }

main :: IO ()
main = quickHttpServe $ route [("/artist/find-latest", expose Artist.findLatest)
                              ,("/artist/create", expose Artist.create)

                              ,("/label/find-latest", expose Label.findLatest)
                              ]
