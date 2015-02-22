{-# LANGUAGE OverloadedStrings #-}
module DbTests.Place (placeTests) where

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

placeTests :: TestTree
placeTests = testGroup "Place"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "place"
  insertPlace
  getPlace
  (flip (set placeId))
  (Place Nothing "TeeTurtle" (Just 1))
