{-# LANGUAGE OverloadedStrings #-}
module Wrappers (tests) where

import Control.Lens
import Data.Text.Lens
import MusicBrainz
import Test.MusicBrainz

import qualified Data.Map as Map
import qualified MusicBrainz.Data.ArtistCredit as MB

import qualified MusicBrainz.API.ArtistCredit as ArtistCredit

import Common

--------------------------------------------------------------------------------
tests :: [Test]
tests =
  [ testExpandCredits ]


--------------------------------------------------------------------------------
testExpandCredits :: Test
testExpandCredits = testCase "Expand credits" $ do
  editor <- registerEditor

  [a1, a2, a3] <- mapM (mkArtist editor) ["Artist 1", "Artist 2", "Artist 3"]

  let ac1 = [ArtistCreditName a1 "Artist 1" " &", ArtistCreditName a2 "Artist 2" "" ]
  let ac2 = [ArtistCreditName a3 "Artist 3" "" ]
  [ac1_id, ac2_id] <- mapM MB.getRef [ac1, ac2]

  expanded <- success ArtistCredit.expandCredits
    [ (["indices"], "0,1")
    , (["0"], ac1_id ^. to dereference . to show . packed)
    , (["1"], ac2_id ^. to dereference . to show . packed)
    ]

  expanded @?= Map.fromList
    [ (ac1_id, ac1)
    , (ac2_id, ac2)
    ]
