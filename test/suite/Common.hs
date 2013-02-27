{-# LANGUAGE OverloadedStrings #-}
module Common where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import MusicBrainz
import Test.MusicBrainz
import Text.Digestive

import qualified Data.Text as Text
import qualified MusicBrainz.Data as MB
import qualified MusicBrainz.Data.Editor as MB

--------------------------------------------------------------------------------
registerEditor :: MusicBrainz (Entity Editor)
registerEditor = MB.register (Editor "ocharles" "password")


--------------------------------------------------------------------------------
mkArtist :: Entity Editor -> Text -> MusicBrainz (Ref Artist)
mkArtist editor name = fmap coreRef $ autoEdit $
  MB.create (entityRef editor)
    (ArtistTree { artistData = Artist { artistName = name
                                      , artistSortName = name
                                      , artistComment = ""
                                      , artistBeginDate = emptyDate
                                      , artistEndDate = emptyDate
                                      , artistEnded = False
                                      , artistGender = Nothing
                                      , artistType = Nothing
                                      , artistCountry = Nothing
                                      }
                , artistRelationships = mempty
                , artistAliases = mempty
                , artistIpiCodes = mempty
                , artistAnnotation = ""
                }) >>= MB.viewRevision


--------------------------------------------------------------------------------
mockAcFields :: [ArtistCreditName] -> [([Text], Text)]
mockAcFields names =
    concat [ [( ["artist-credits", "indices"] , formatIndices names) ]
           , mkFields artistCreditField names
           ]
  where
    artistCreditField i (ArtistCreditName artist name joinPhrase) =
      [ (["artist-credits", i, "artist"], (dereference artist) ^. re mbid . packed)
      , (["artist-credits", i, "name"], name)
      , (["artist-credits", i, "join-phrase"], joinPhrase)
      ]


--------------------------------------------------------------------------------
mockPost :: Form Text MusicBrainz a -> [([Text], Text)] -> MusicBrainz (View Text, Maybe a)
mockPost form submissions = postForm "" form env
  where
    env p = return $ maybe [] (return . TextInput) $ lookup (drop 1 p) submissions


--------------------------------------------------------------------------------
success :: (Eq a, Show a) => Form Text MusicBrainz a -> [([Text], Text)] -> MusicBrainz a
success form submissions = do
  (v, actual) <- mockPost form submissions
  return $ fromMaybe (error $ show v) actual


--------------------------------------------------------------------------------
expectFailure :: (Eq a, Show a) => Form Text MusicBrainz a -> [([Text], Text)] -> MusicBrainz ()
expectFailure form submissions = do
  (_, actual) <- mockPost form submissions
  actual @?= Nothing


--------------------------------------------------------------------------------
testForm :: (Eq a, Show a) => Form Text MusicBrainz a -> [([Text], Text)] -> a -> MusicBrainz ()
testForm form submissions expected = do
  actual <- success form submissions
  actual @?= expected


--------------------------------------------------------------------------------
formatIndices :: [a] -> Text
formatIndices x = Text.intercalate "," $ [0..pred $ length x] ^.. traverse.to show.packed


--------------------------------------------------------------------------------
mkFields :: (Text -> a -> [([Text], Text)]) -> [a] -> [([Text], Text)]
mkFields f x = concat $ zipWith f (map (view packed . show) [0..]) x
