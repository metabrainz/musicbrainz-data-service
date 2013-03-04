{-# LANGUAGE OverloadedStrings #-}
module Handlers (tests) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Monoid
import MusicBrainz
import Snap.Core (rspStatus)
import Snap.Snaplet
import Snap.Test
import Test.MusicBrainz

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

import MusicBrainz.Service

--------------------------------------------------------------------------------
tests :: [Test]
tests = map testHandlerExists
  [ "/open-session"
  , "/abort-session"
  , "/close-session"
  , "/artist/create"
  , "/artist/eligible-for-cleanup"
  , "/artist/find-latest"
  , "/artist/get-revision"
  , "/artist/merge"
  , "/artist/update"
  , "/artist/view-aliases"
  , "/artist/view-annotation"
  , "/artist/view-ipi-codes"
  , "/artist/view-relationships"
  , "/artist/view-revision"
  , "/artist-credit/expand"
  , "/artist-type/add"
  , "/edit/add-note"
  , "/edit/open"
  , "/gender/add"
  , "/iswc/find-by-works"
  , "/label/create"
  , "/label/eligible-for-cleanup"
  , "/label/find-latest"
  , "/label/get-revision"
  , "/label/merge"
  , "/label/update"
  , "/label/view-aliases"
  , "/label/view-annotation"
  , "/label/view-ipi-codes"
  , "/label/view-relationships"
  , "/label/view-revision"
  , "/recording/create"
  , "/recording/find-by-artist"
  , "/recording/find-latest"
  , "/recording/find-recording-tracks"
  , "/recording/get-revision"
  , "/recording/merge"
  , "/recording/update"
  , "/recording/view-annotation"
  , "/recording/view-relationships"
  , "/recording/view-revision"
  , "/release/create"
  , "/release/find-by-artist"
  , "/release/find-by-label"
  , "/release/find-by-release-group"
  , "/release/find-latest"
  , "/release/get-revision"
  , "/release/merge"
  , "/release/update"
  , "/release/view-annotation"
  , "/release/view-mediums"
  , "/release/view-relationships"
  , "/release/view-release-labels"
  , "/release/view-revision"
  , "/release-group/create"
  , "/release-group/find-by-artist"
  , "/release-group/find-latest"
  , "/release-group/get-revision"
  , "/release-group/merge"
  , "/release-group/update"
  , "/release-group/view-annotation"
  , "/release-group/view-relationships"
  , "/release-group/view-revision"
  , "/url/create"
  , "/url/find-latest"
  , "/url/get-revision"
  , "/url/merge"
  , "/url/update"
  , "/url/view-relationships"
  , "/url/view-revision"
  , "/work/create"
  , "/work/eligible-for-cleanup"
  , "/work/find-by-artist"
  , "/work/find-latest"
  , "/work/get-revision"
  , "/work/merge"
  , "/work/update"
  , "/work/view-aliases"
  , "/work/view-annotation"
  , "/work/view-relationships"
  , "/work/view-revision"
  ]


--------------------------------------------------------------------------------
testHandlerExists :: ByteString -> Test
testHandlerExists path = testCase (Text.unpack $ (Encoding.decodeUtf8 path) `Text.append` " exists") $ do
  testSessionStore <- liftIO $
    atomically $ newTVar mempty

  (_, s, tearDown) <- liftIO $
    runSnaplet (Just "test") $
      serviceInit (return defaultConnectInfo)
                  (return testSessionStore)

  response <- runHandler (postRaw path "application/json" mempty) s
  assertBool "Response code is >= 500" $ rspStatus response < 500
  assertBool "Response code is 404" $ rspStatus response /= 404

  liftIO tearDown

