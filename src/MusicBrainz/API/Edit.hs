{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.API.Edit
    ( addEditNote
    , open
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import MusicBrainz
import MusicBrainz.API
import MusicBrainz.API.JSON (RefObject(..))
import qualified MusicBrainz.Data.Edit as MB

--------------------------------------------------------------------------------
open :: Form Text MusicBrainz (RefObject Edit)
open = runApi $ pure (RefObject <$> MB.openEdit)


--------------------------------------------------------------------------------
addEditNote :: Form Text MusicBrainz ()
addEditNote = runApi $
    MB.addEditNote <$> edit
                   <*> editNote
  where
    editNote = EditNote <$> "text" .: nonEmptyText
                        <*> editor
