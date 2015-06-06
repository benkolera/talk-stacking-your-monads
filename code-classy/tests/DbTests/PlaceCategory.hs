{-# LANGUAGE OverloadedStrings #-}
module DbTests.PlaceCategory (placeCategoryTests) where

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

placeCategoryTests :: TestTree
placeCategoryTests = testGroup "PlaceCategory"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "place_category"
  insertPlaceCategory
  getPlaceCategory
  (flip (set placeCategoryId))
  (PlaceCategory Nothing "TShirts")
