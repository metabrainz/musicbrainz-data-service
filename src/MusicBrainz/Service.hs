{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Service
    ( serviceInitAutomatic
    , serviceInit
    , openSession
    , emptySessionStore
    ) where

import           Control.Applicative ((<*>), (<$>), pure)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (TVar, TMVar, atomically, newTVar, newTMVar, readTVar, takeTMVar, putTMVar, writeTVar, modifyTVar, tryReadTMVar)
import           Control.Error.Util (note)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever, forM, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (EitherT(..), eitherT, hoistEither, left)
import           Data.Aeson (decode, encode, object, (.=))
import           Data.Configurator (lookupDefault)
import           Data.Foldable (forM_)
import           Data.Maybe (catMaybes)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import           Data.Traversable (traverse)
import           Snap (Initializer, SnapletInit, makeSnaplet, Handler, getSnapletUserConfig, addRoutes)
import           Snap.Core (MonadSnap, writeLBS, setContentType, modifyResponse, setResponseCode, readRequestBody, getsRequest, getHeader)
import           Snap.Snaplet.Session.Common (mkCSRFToken, mkRNG, RNG)
import           Text.Digestive (Form)
import           Text.Digestive.Aeson (digestJSON, jsonErrors)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified MusicBrainz.API.Artist as Artist
import qualified MusicBrainz.API.ArtistCredit as ArtistCredit
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
requestSession :: EitherT (Handler Service Service ()) (Handler Service Service) Text
requestSession =
    EitherT $ note invalidToken . fmap Encoding.decodeUtf8 <$>
      getsRequest (getHeader "MB-Session")
  where
    invalidToken =
      writeClientError "The MB-Session header was required but could not be parsed"


--------------------------------------------------------------------------------
lockSession :: EitherT (Handler Service Service ()) (Handler Service Service) Session
lockSession = do
    token <- requestSession
    sessionStore <- lift $ gets openSessions

    EitherT $ note (noSession token) <$> do
      liftIO $ atomically $ do
        ss <- readTVar sessionStore
        traverse takeTMVar $ Map.lookup token ss
  where
    noSession token =
      writeClientError $ T.concat
        [ "No session could be found with the requested MB-Session token '"
        , token
        , "'" ]

--------------------------------------------------------------------------------
unlockSession :: Session -> EitherT (Handler Service Service ()) (Handler Service Service) ()
unlockSession session = do
    token <- requestSession
    sessionStore <- lift $ gets openSessions

    liftIO $ do
      now <- getCurrentTime
      atomically $ do
        ss <- readTVar sessionStore
        putTMVar (ss Map.! token) session { lastUsed = now }


--------------------------------------------------------------------------------
expose :: TopLevel a => Form Text MusicBrainz a -> Handler Service Service ()
expose f = do
  modifyResponse (setContentType "application/json")
  eitherT id id $ do
    -- Decode the JSON before trying to get a session lock, so we minimize the
    -- amount of time the session is exclusively locked.
    json' <- EitherT $ note failedTeDecode . decode <$> readRequestBody (1024*1024)

    session <- lockSession

    -- We run the 'form' that validates the users submitted parameters to the API
    -- call.
    outcome <- liftIO (try (runMbContext (sessionContext session) (digestJSON f json')))

    -- Release our lock on the session, and bump up the time it was last used.
    unlockSession session

    case outcome of
      Left (exception :: SomeException) -> do
        -- There was indeed an exception, so lets render that back to the
        -- client.
        left (writeServerError $ T.pack $ show exception)

      Right (view, ret) ->
        hoistEither $ note (invalidParameters view) $ fmap (writeLBS . encode) ret
  where
    failedTeDecode = writeClientError "Could not parse JSON"

    invalidParameters view = do
      modifyResponse (setResponseCode 400)
      writeLBS (encode $ jsonErrors view)


--------------------------------------------------------------------------------
writeClientError :: MonadSnap m => Text -> m ()
writeClientError = writeError 400

writeServerError :: MonadSnap m => Text -> m ()
writeServerError = writeError 500

writeError :: MonadSnap m => Int -> Text -> m ()
writeError status e = do
   modifyResponse (setResponseCode status)
   writeLBS . encode $ object [ "error" .= (e::Text) ]


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
closeSessionWith :: MusicBrainz () -> Handler Service Service ()
closeSessionWith f = eitherT id (const $ return ()) $ do
    token <- requestSession
    sessionStore <- lift $ gets openSessions

    liftIO $ do
      context <- atomically $ do
        allSessions <- readTVar sessionStore
        case Map.lookup token allSessions of
          Just session -> do
            writeTVar sessionStore (Map.delete token allSessions)
            Just <$> takeTMVar session
          Nothing -> return Nothing

      forM_ context $ \c -> runMbContext (sessionContext c) f

    lift $ writeLBS . encode $ object []


--------------------------------------------------------------------------------
closeSession :: Handler Service Service ()
closeSession = closeSessionWith MusicBrainz.commit


--------------------------------------------------------------------------------
abortSession :: Handler Service Service ()
abortSession = closeSessionWith MusicBrainz.rollback


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
      , ("/abort-session", abortSession)
      , ("/close-session", closeSession)

      , ("/artist/create", expose Artist.create)
      , ("/artist/eligible-for-cleanup", expose Artist.eligibleForCleanup)
      , ("/artist/find-latest", expose Artist.findLatest)
      , ("/artist/get-revision", expose Artist.getRevision)
      , ("/artist/merge", expose Artist.merge)
      , ("/artist/update", expose Artist.update)
      , ("/artist/view-aliases", expose Artist.viewAliases)
      , ("/artist/view-annotation", expose Artist.viewAnnotation)
      , ("/artist/view-ipi-codes", expose Artist.viewIpiCodes)
      , ("/artist/view-relationships", expose Artist.viewRelationships)
      , ("/artist/view-revision", expose Artist.viewRevision)

      , ("/artist-credit/expand", expose ArtistCredit.expandCredits)

      , ("/artist-type/add", expose ArtistType.add)

      , ("/edit/add-note", expose Edit.addEditNote)
      , ("/edit/open", expose Edit.open)

      , ("/gender/add", expose Gender.add)

      , ("/iswc/find-by-works", expose Iswc.findByWorks)

      , ("/label/create", expose Label.create)
      , ("/label/eligible-for-cleanup", expose Label.eligibleForCleanup)
      , ("/label/find-latest", expose Label.findLatest)
      , ("/label/get-revision", expose Label.getRevision)
      , ("/label/merge", expose Label.merge)
      , ("/label/update", expose Label.update)
      , ("/label/view-aliases", expose Label.viewAliases)
      , ("/label/view-annotation", expose Label.viewAnnotation)
      , ("/label/view-ipi-codes", expose Label.viewIpiCodes)
      , ("/label/view-relationships", expose Label.viewRelationships)
      , ("/label/view-revision", expose Label.viewRevision)

      , ("/recording/create", expose Recording.create)
      , ("/recording/find-by-artist", expose Recording.findByArtist)
      , ("/recording/find-by-isrc", expose Recording.findByIsrc)
      , ("/recording/find-latest", expose Recording.findLatest)
      , ("/recording/find-recording-tracks", expose Recording.findRecordingTracks)
      , ("/recording/get-revision", expose Recording.getRevision)
      , ("/recording/merge", expose Recording.merge)
      , ("/recording/update", expose Recording.update)
      , ("/recording/view-annotation", expose Recording.viewAnnotation)
      , ("/recording/view-isrcs", expose Recording.viewIsrcs)
      , ("/recording/view-relationships", expose Recording.viewRelationships)
      , ("/recording/view-revision", expose Recording.viewRevision)

      , ("/release/create", expose Release.create)
      , ("/release/find-by-artist", expose Release.findByArtist)
      , ("/release/find-by-release-group", expose Release.findByReleaseGroup)
      , ("/release/find-by-label", expose Release.findByLabel)
      , ("/release/find-latest", expose Release.findLatest)
      , ("/release/get-revision", expose Release.getRevision)
      , ("/release/merge", expose Release.merge)
      , ("/release/update", expose Release.update)
      , ("/release/view-annotation", expose Release.viewAnnotation)
      , ("/release/view-mediums", expose Release.viewMediums)
      , ("/release/view-release-labels", expose Release.viewReleaseLabels)
      , ("/release/view-relationships", expose Release.viewRelationships)
      , ("/release/view-revision", expose Release.viewRevision)

      , ("/release-group/create", expose ReleaseGroup.create)
      , ("/release-group/find-by-artist", expose ReleaseGroup.findByArtist)
      , ("/release-group/find-latest", expose ReleaseGroup.findLatest)
      , ("/release-group/get-revision", expose ReleaseGroup.getRevision)
      , ("/release-group/merge", expose ReleaseGroup.merge)
      , ("/release-group/update", expose ReleaseGroup.update)
      , ("/release-group/view-annotation", expose ReleaseGroup.viewAnnotation)
      , ("/release-group/view-relationships", expose ReleaseGroup.viewRelationships)
      , ("/release-group/view-revision", expose ReleaseGroup.viewRevision)

      , ("/url/create", expose Url.create)
      , ("/url/find-latest", expose Url.findLatest)
      , ("/url/get-revision", expose Url.getRevision)
      , ("/url/merge", expose Url.merge)
      , ("/url/update", expose Url.update)
      , ("/url/view-relationships", expose Url.viewRelationships)
      , ("/url/view-revision", expose Url.viewRevision)

      , ("/work/create", expose Work.create)
      , ("/work/eligible-for-cleanup", expose Work.eligibleForCleanup)
      , ("/work/find-by-artist", expose Work.findByArtist)
      , ("/work/find-latest", expose Work.findLatest)
      , ("/work/get-revision", expose Work.getRevision)
      , ("/work/merge", expose Work.merge)
      , ("/work/update", expose Work.update)
      , ("/work/view-aliases", expose Work.viewAliases)
      , ("/work/view-annotation", expose Work.viewAnnotation)
      , ("/work/view-relationships", expose Work.viewRelationships)
      , ("/work/view-revision", expose Work.viewRevision)
      ]

    s <- sessionStore
    liftIO $ forkIO $ reaper s

    Service <$> connInfo
            <*> pure s
            <*> liftIO mkRNG


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
        c' <- tryReadTMVar c
        case c' of
          Just inactiveSession ->
            return $
              if isStale inactiveSession
                then Just (token, inactiveSession)
                else Nothing
          Nothing -> return Nothing

    writeTVar sessionStore (foldl (flip Map.delete) sessions $ map fst dead)
    return $ map snd dead

  forM_ toReap $ \c -> runMbContext (sessionContext c) rollback
