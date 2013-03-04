{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ArtistCredit where

import Control.Applicative
import Data.Text hiding (map)
import Text.Digestive

import           MusicBrainz hiding (ArtistCredit, coreRef, releaseLabel, partialDate)
import           MusicBrainz.API
import           MusicBrainz.API.JSON (ArtistCredit(..))
import qualified MusicBrainz as MB
import qualified MusicBrainz.Data.ArtistCredit as MB

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
expandCredits :: Form Text MusicBrainz
                       (Map.Map (Ref MB.ArtistCredit) [ArtistCreditName])
expandCredits =
  runApi $ MB.expandCredits . Set.fromList <$>
    listOf (const $ ref "Could not resolve artist credit") Nothing



--------------------------------------------------------------------------------
allCredits :: Form Text MusicBrainz [ArtistCredit]
allCredits = fmap (map ArtistCredit) $ runApi $ MB.allArtistCredits <$> "artist" .: coreRef
