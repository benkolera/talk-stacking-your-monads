{-# LANGUAGE OverloadedStrings #-}
module DbTests.TransactionVisa (transactionVisaTests) where

import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

transactionVisaTests :: TestTree
transactionVisaTests = testGroup "TransactionVisa"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "transaction_visa"
  insertTransactionVisa
  getTransactionVisa
  (\ n _ -> n)
  (TransactionVisa 1 (fromGregorian 2015 2 9) "AUD" "AU")
