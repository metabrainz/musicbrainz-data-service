{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.API.Update
    ( update
    ) where

import Control.Applicative
import Data.Text (Text)
import Text.Digestive

import MusicBrainz
import MusicBrainz.API
import MusicBrainz.API.JSON
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Edit as MB

update :: (MB.ResolveReference (Revision a), MB.Update a)
       => Form Text MusicBrainz (Tree a)
       -> Form Text MusicBrainz (RefObject (Revision a))
update tree =
  fmap RefObject $ runApi $
    MB.withEdit
      <$> "edit" .: edit
      <*> (MB.update
             <$> editor
             <*> revision
             <*> tree)

