{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Db.PlaceCategory
  ( PlaceCategory'(PlaceCategory)
  , NewPlaceCategory
  , PlaceCategory
  , placeCategoryQuery
  , getPlaceCategory
  , insertPlaceCategory
  , placeCategoryId
  , placeCategoryName
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Opaleye

import Db.Internal

data PlaceCategory' a b = PlaceCategory
  { _placeCategoryId   :: a
  , _placeCategoryName :: b
  } deriving (Eq,Show)
makeLenses ''PlaceCategory'

type PlaceCategory = PlaceCategory' Int Text
type PlaceCategoryColumn = PlaceCategory' (Column PGInt4) (Column PGText)

makeAdaptorAndInstance "pPlaceCategory" ''PlaceCategory'

type NewPlaceCategory = PlaceCategory' (Maybe Int) Text

type NewPlaceCategoryColumn = PlaceCategory' (Maybe (Column PGInt4)) (Column PGText)

placeCategoryTable :: Table NewPlaceCategoryColumn PlaceCategoryColumn
placeCategoryTable = Table "place_category" $ pPlaceCategory PlaceCategory
  { _placeCategoryId     = optional "id"
  , _placeCategoryName   = required "name"
  }

placeCategoryQuery :: Query PlaceCategoryColumn
placeCategoryQuery = queryTable placeCategoryTable

insertPlaceCategory :: CanDb c e m => NewPlaceCategory -> m Int
insertPlaceCategory =
  liftInsertReturningFirst placeCategoryTable (view placeCategoryId) . packNew

getPlaceCategory :: CanDb c e m => Int -> m (Maybe PlaceCategory)
getPlaceCategory i = liftQueryFirst $ proc () -> do
  p <- placeCategoryQuery -< ()
  restrict -< p^.placeCategoryId .== pgInt4 i
  returnA -< p

packNew :: NewPlaceCategory -> NewPlaceCategoryColumn
packNew = pPlaceCategory PlaceCategory
  { _placeCategoryId     = fmap pgInt4
  , _placeCategoryName   = pgStrictText
  }
