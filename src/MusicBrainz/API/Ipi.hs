{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Ipi ( findByLabels ) where

import Control.Applicative
import Data.Map
import Data.Text
import Text.Digestive

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.API
import qualified MusicBrainz.Data as MB

findByLabels :: Form Text MusicBrainz (Map (Ref (Revision Label)) (Set.Set IPI))
findByLabels = runApi $
  MB.findIpiCodes <$> (Set.fromList <$> "revisions" .: listOf (const revision) Nothing)
