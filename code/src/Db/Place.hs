{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Db.Place
  ( Place'(Place)
  , NewPlace
  , Place
  , placeQuery
  , getPlace
  , insertPlace
  , upsertPlaceByName
  , placeId
  , placeName
  , placePlaceCategoryId
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Opaleye

import Db.Internal

data Place' a b c = Place
  { _placeId              :: a
  , _placeName            :: b
  , _placePlaceCategoryId :: c
  } deriving (Eq,Show)
makeLenses ''Place'

type Place = Place' Int Text (Maybe Int)
type PlaceColumn = Place' (Column PGInt4) (Column PGText) (Column (Nullable PGInt4))

makeAdaptorAndInstance "pPlace" ''Place'

type NewPlace = Place' (Maybe Int) Text (Maybe Int)

type NewPlaceColumn = Place' (Maybe (Column PGInt4)) (Column PGText) (Column (Nullable PGInt4))

placeTable :: Table NewPlaceColumn PlaceColumn
placeTable = Table "place" $ pPlace Place
  { _placeId              = optional "id"
  , _placeName            = required "name"
  , _placePlaceCategoryId = required "place_category_id"
  }

placeQuery :: Query PlaceColumn
placeQuery = queryTable placeTable

insertPlace :: NewPlace -> Db Int
insertPlace =
  liftInsertReturningFirst placeTable (view placeId) . packNew

findPlaceByName :: Text -> Db (Maybe Place)
findPlaceByName n = liftQueryFirst $ proc () -> do
  a <- placeQuery -< ()
  restrict -< a^.placeName .== pgStrictText n
  returnA -< a

upsertPlaceByName :: NewPlace -> Db Int
upsertPlaceByName na = do
  a <- findPlaceByName (na^.placeName)
  maybe (insertPlace na) (pure . (^.placeId)) a

getPlace :: Int -> Db (Maybe Place)
getPlace i = liftQueryFirst $ proc () -> do
  p <- placeQuery -< ()
  restrict -< p^.placeId .== pgInt4 i
  returnA -< p

packNew :: NewPlace -> NewPlaceColumn
packNew = pPlace Place
  { _placeId              = fmap pgInt4
  , _placeName            = pgStrictText
  , _placePlaceCategoryId = maybeToNullable . fmap pgInt4
  }
