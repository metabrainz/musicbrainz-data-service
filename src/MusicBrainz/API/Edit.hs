{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Edit
    ( open
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import MusicBrainz
import MusicBrainz.API
import MusicBrainz.API.JSON (RefObject(..))
import qualified MusicBrainz.Data.Edit as Edit

open :: Form Text MusicBrainz (RefObject Edit)
open = runApi $ pure (RefObject <$> Edit.openEdit)
