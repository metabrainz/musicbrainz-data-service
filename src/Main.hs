{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (encode, ToJSON)
import           Data.Text (Text)
import           Snap.Core (route, writeLBS, setContentType, modifyResponse, MonadSnap)
import           Snap.Http.Server (quickHttpServe)
import           Text.Digestive (Method(Post), Form)
import           Text.Digestive.Snap (method, defaultSnapFormConfig, runFormWith)
import           Text.Digestive.View (childErrors)

import qualified MusicBrainz.API.Artist as Artist

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, runMb, MusicBrainz)
import           MusicBrainz.API.JSON ()

expose :: (MonadSnap m, ToJSON a) => Form Text m (MusicBrainz a) -> m ()
expose f = do
  (view, ret) <- runFormWith defaultSnapFormConfig { method = Just Post } "api" f
  case ret of
    Just r -> do
      liftIO (runMbAction r) >>= writeLBS . encode
      modifyResponse (setContentType "application/json")
    Nothing -> writeLBS (encode $ childErrors "" view)

runMbAction :: MusicBrainz a -> IO a
runMbAction = runMb defaultConnectInfo { connectDatabase = "musicbrainz_nes", connectUser = "musicbrainz" }

main :: IO ()
main = quickHttpServe $ route [("/artist/find-latest-by-mbid", expose Artist.findLatestByMbid)]
