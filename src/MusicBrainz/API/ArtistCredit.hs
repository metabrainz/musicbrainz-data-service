{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.ArtistCredit where

import Control.Applicative
import Data.Text
import Text.Digestive

import           MusicBrainz hiding (coreRef, releaseLabel, partialDate)
import           MusicBrainz.API
import qualified MusicBrainz.Data.ArtistCredit as MB

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
expandCredits :: Form Text MusicBrainz
                       (Map.Map (Ref ArtistCredit) [ArtistCreditName])
expandCredits =
  runApi $ MB.expandCredits . Set.fromList <$>
    listOf (const $ ref "Could not resolve artist credit") Nothing
