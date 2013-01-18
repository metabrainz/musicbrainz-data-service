module MusicBrainz.API.Iswc ( findByWorks ) where

import Control.Applicative
import Data.Map
import Data.Monoid
import Data.Text
import Text.Digestive

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.API
import qualified MusicBrainz.Data.Work as MB

findByWorks :: Form Text MusicBrainz (Map (Ref (Revision Work)) (Set.Set ISWC))
findByWorks = runApi $
  MB.findIswcs <$> pure mempty
    -- (Set.fromList <$> "revisions" .: listOf ("revision" :. iswc))
