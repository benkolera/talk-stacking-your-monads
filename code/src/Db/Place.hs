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
  , placeId
  , placeName
  , placePlaceCategoryId
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Control.Monad.Except       (MonadError)
import Control.Monad.Reader       (MonadReader)
import Control.Monad.Trans        (MonadIO)
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

insertPlace
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    )
  => NewPlace
  -> m [Int]
insertPlace =
  liftInsertReturning placeTable (view placeId) . packNew

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
