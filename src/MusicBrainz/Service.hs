{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Service (serviceInit, serviceInitContext) where

import           Control.Applicative ((<*>), (<$>))
import           Control.Concurrent.STM (TVar, TMVar, atomically, newTVar, newTMVar, readTVar, takeTMVar, putTMVar, writeTVar, modifyTVar)
import           Control.Exception (SomeException, try)
import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Aeson (decode, encode, Value, object, (.=))
import           Data.Configurator (lookupDefault)
import           Data.Foldable (forM_)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Snap (Initializer, SnapletInit, makeSnaplet, Handler, getSnapletUserConfig, addRoutes)
import           Snap.Core (writeLBS, setContentType, modifyResponse, setResponseCode, readRequestBody, getsRequest, getHeader)
import           Snap.Snaplet.Session.Common (mkCSRFToken, mkRNG, RNG)
import           Text.Digestive (Form)
import           Text.Digestive.Aeson (digestJSON, jsonErrors)

import qualified Data.Map as Map
import qualified Data.Text.Encoding as Encoding
import qualified MusicBrainz.API.Artist as Artist
import qualified MusicBrainz.API.ArtistType as ArtistType
import qualified MusicBrainz.API.Edit as Edit
import qualified MusicBrainz.API.Gender as Gender
import qualified MusicBrainz.API.Iswc as Iswc
import qualified MusicBrainz.API.Label as Label
import qualified MusicBrainz.API.Recording as Recording
import qualified MusicBrainz.API.Release as Release
import qualified MusicBrainz.API.ReleaseGroup as ReleaseGroup
import qualified MusicBrainz.API.Url as Url
import qualified MusicBrainz.API.Work as Work

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, connectPassword, MusicBrainz, Context, openContext, runMbContext, begin, ConnectInfo(..), commit)
import           MusicBrainz.API.JSON (TopLevel)

--------------------------------------------------------------------------------
expose :: TopLevel a => Form Text MusicBrainz a -> Handler Service Service ()
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
      Just token <- fmap (fmap Encoding.decodeUtf8) $ getsRequest (getHeader "MB-Session")
      sessionStore <- gets openSessions
      context <- liftIO $ atomically $ do
        ss <- readTVar sessionStore
        takeTMVar (ss Map.! token)

      outcome <- liftIO (try (runMbContext context (digestJSON f json')))

      liftIO $ atomically $ do
        ss <- readTVar sessionStore
        putTMVar (ss Map.! token) context

      modifyResponse (setContentType "application/json")
      case outcome of
        Left (exception :: SomeException) -> do
          -- There was indeed an exception, so lets render that back to the
          -- client.
          modifyResponse (setResponseCode 500)
          writeLBS . encode $ object [ "error" .= show exception ]

        Right (view, ret') ->
          case ret' of
            Just success ->
              -- All went smoothly, so just render back the API result.
              writeLBS (encode success)

            Nothing -> do
              -- The client hasn't submitted valid parameters, so we'll render back
              -- a list of all the parameters that failed validation.
              modifyResponse (setResponseCode 400)
              writeLBS (encode $ jsonErrors view)


--------------------------------------------------------------------------------
type SessionToken = Text


--------------------------------------------------------------------------------
data Service = Service { connectInfo :: ConnectInfo
                       , openSessions :: TVar (Map.Map SessionToken (TMVar Context))
                       , rng :: RNG
                       }


--------------------------------------------------------------------------------
openSession :: Handler Service Service ()
openSession = do
  -- TODO Handle collisions
  token <- gets rng >>= liftIO . mkCSRFToken

  sessionStore <- gets openSessions
  context <- gets connectInfo >>= openContext

  runMbContext context begin

  liftIO $ atomically $ do
    s <- newTMVar context
    modifyTVar sessionStore (Map.insert token s)

  writeLBS . encode $ object [ "token" .= token ]


--------------------------------------------------------------------------------
closeSession :: Handler Service Service ()
closeSession = do
  Just token <- fmap (fmap Encoding.decodeUtf8) $ getsRequest (getHeader "MB-Session")

  sessionStore <- gets openSessions

  context <- liftIO $ atomically $ do
    allSessions <- readTVar sessionStore
    case Map.lookup token allSessions of
      Just session -> do
        writeTVar sessionStore (Map.delete token allSessions)
        Just <$> takeTMVar session
      Nothing -> return Nothing

  forM_ context $ \c -> runMbContext c (MusicBrainz.commit)


--------------------------------------------------------------------------------
serviceInit :: SnapletInit Service Service
serviceInit = serviceInitContext $ do
  config <- getSnapletUserConfig
  [db, user, pass] <- liftIO $ forM
    [ ("database", connectDatabase)
    , ("username", connectUser)
    , ("password", connectPassword)
    ] $ \(k, def) -> lookupDefault (def defaultConnectInfo) config k
  return $ defaultConnectInfo { connectDatabase = db
                              , connectUser = user
                              , connectPassword = pass
                              }

--------------------------------------------------------------------------------
serviceInitContext :: Initializer Service Service ConnectInfo -> SnapletInit Service Service
serviceInitContext ctxInit = makeSnaplet "service" "musicbrainz-data HTTP service" Nothing $ do
  addRoutes
    [ ("/open-session", openSession)
    , ("/close-session", closeSession)

    , ("/artist/create", expose Artist.create)
    , ("/artist/find-latest", expose Artist.findLatest)
    , ("/artist/view-revision", expose Artist.viewRevision)

    , ("/artist-type/add", expose ArtistType.add)

    , ("/edit/add-note", expose Edit.addEditNote)
    , ("/edit/open", expose Edit.open)

    , ("/gender/add", expose Gender.add)

    , ("/iswc/find-by-works", expose Iswc.findByWorks)

    , ("/label/create", expose Label.create)
    , ("/label/find-latest", expose Label.findLatest)
    , ("/label/view-revision", expose Label.viewRevision)

    , ("/recording/find-latest", expose Recording.findLatest)
    , ("/recording/view-revision", expose Recording.viewRevision)

    , ("/release/find-latest", expose Release.findLatest)
    , ("/release/view-revision", expose Release.viewRevision)

    , ("/release-group/create", expose ReleaseGroup.create)
    , ("/release-group/view-revision", expose ReleaseGroup.viewRevision)

    , ("/url/find-latest", expose Url.findLatest)
    , ("/url/view-revision", expose Url.viewRevision)

    , ("/work/create", expose Work.create)
    , ("/work/eligible-for-cleanup", expose Work.eligibleForCleanup)
    , ("/work/find-latest", expose Work.findLatest)
    , ("/work/update", expose Work.update)
    , ("/work/view-aliases", expose Work.viewAliases)
    , ("/work/view-annotation", expose Work.viewAnnotation)
    , ("/work/view-revision", expose Work.viewRevision)
    ]

  Service <$> ctxInit
          <*> liftIO (atomically $ newTVar mempty)
          <*> liftIO mkRNG
