{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.API.Create
    ( create
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import MusicBrainz
import MusicBrainz.API
import MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

create :: MB.Create a
       => Form Text MusicBrainz (Tree a)
       -> Form Text MusicBrainz (RefObject (Revision a))
create tree =
  fmap RefObject $ runApi $
    MB.withEdit
      <$> "edit" .: edit
      <*> (MB.create
             <$> editor
             <*> tree)

