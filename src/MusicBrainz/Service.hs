{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Service
    ( serviceInitAutomatic
    , serviceInit
    , unsafeRequestContext
    , openSession
    , emptySessionStore
    ) where

import           Control.Applicative ((<*>), (<$>), pure)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (TVar, TMVar, atomically, newTVar, newTMVar, readTVar, takeTMVar, putTMVar, writeTVar, modifyTVar, tryReadTMVar)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever, forM, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Aeson (decode, encode, Value, object, (.=))
import           Data.Configurator (lookupDefault)
import           Data.Foldable (forM_)
import           Data.Maybe (catMaybes)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Data.Time (UTCTime, getCurrentTime, diffUTCTime)
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

import           MusicBrainz (defaultConnectInfo, connectUser, connectDatabase, connectPassword, MusicBrainz, Context, openContext, runMbContext, begin, ConnectInfo(..), commit, rollback)
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
      session <- liftIO $ atomically $ do
        ss <- readTVar sessionStore
        takeTMVar (ss Map.! token)

      outcome <- liftIO (try (runMbContext (sessionContext session) (digestJSON f json')))

      now <- liftIO getCurrentTime
      liftIO $ atomically $ do
        ss <- readTVar sessionStore
        putTMVar (ss Map.! token) session { lastUsed = now }

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
type SessionStore = TVar (Map.Map SessionToken (TMVar Session))

data Service = Service { connectInfo :: ConnectInfo
                       , openSessions :: SessionStore
                       , rng :: RNG
                       }


--------------------------------------------------------------------------------
data Session = Session { lastUsed :: UTCTime
                       , sessionContext :: Context
                       }


--------------------------------------------------------------------------------
openSession :: Handler Service Service (SessionToken, Context)
openSession = do
  -- TODO Handle collisions
  token <- gets rng >>= liftIO . mkCSRFToken

  sessionStore <- gets openSessions
  context <- gets connectInfo >>= openContext

  runMbContext context begin
  now <- liftIO getCurrentTime

  liftIO $ atomically $ do
    s <- newTMVar (Session now context)
    modifyTVar sessionStore (Map.insert token s)

  writeLBS . encode $ object [ "token" .= token ]
  return (token, context)


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

  forM_ context $ \c -> runMbContext (sessionContext c) (MusicBrainz.commit)


--------------------------------------------------------------------------------
serviceInitAutomatic :: SnapletInit Service Service
serviceInitAutomatic = serviceInit connInfo (liftIO emptySessionStore)
  where
    connInfo = do
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
emptySessionStore :: IO SessionStore
emptySessionStore = atomically $ newTVar mempty


--------------------------------------------------------------------------------
serviceInit :: Initializer Service Service ConnectInfo
            -> Initializer Service Service SessionStore
            -> SnapletInit Service Service
serviceInit connInfo sessionStore =
  makeSnaplet "service" "musicbrainz-data HTTP service" Nothing $ do
    addRoutes
      [ ("/open-session", void openSession)
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

    s <- sessionStore
    liftIO $ forkIO $ reaper s

    Service <$> connInfo
            <*> pure s
            <*> liftIO mkRNG


--------------------------------------------------------------------------------
unsafeRequestContext :: SessionToken -> Handler Service Service Context
unsafeRequestContext token = do
  sessionStore <- gets openSessions
  liftIO $ atomically $ do
    session <- (Map.! token) <$> readTVar sessionStore
    s <- takeTMVar session
    putTMVar session s
    return (sessionContext s)


--------------------------------------------------------------------------------
reaper :: TVar (Map.Map SessionToken (TMVar Session)) -> IO ()
reaper sessionStore = forever $ do
  threadDelay (1 * 1000000)

  now <- getCurrentTime
  let isStale session = now `diffUTCTime` (lastUsed session) > 5

  toReap <- atomically $ do
    sessions <- readTVar sessionStore
    dead <- fmap catMaybes $ forM (Map.assocs sessions) $
      \(token, c) -> do
        c <- tryReadTMVar c
        case c of
          Just inactiveSession ->
            return $
              if isStale inactiveSession
                then Just (token, inactiveSession)
                else Nothing
          Nothing -> return Nothing

    writeTVar sessionStore (foldl (flip Map.delete) sessions $ map fst dead)
    return $ map snd dead

  forM_ toReap $ \c -> runMbContext (sessionContext c) rollback
