{-# LANGUAGE OverloadedStrings #-}
module DbTests.Account (accountTests) where

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

accountTests :: TestTree
accountTests = testGroup "Account"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "account"
  insertAccount
  getAccount
  (flip (set accountId))
  (Account Nothing "Savings" 1234 "Savings Account")
