{-# LANGUAGE OverloadedStrings #-}
module DbTests.Transaction (transactionTests) where

import Control.Lens
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

transactionTests :: TestTree
transactionTests = testGroup "Transaction"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "transaction"
  insertTransaction
  getTransaction
  (flip (set transactionId))
  (Transaction Nothing (fromGregorian 2015 2 23) (-10) 90 "eftpos" (Just 1) 1)
