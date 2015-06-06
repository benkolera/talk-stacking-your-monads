{-# LANGUAGE OverloadedStrings #-}
module DbTests.TransactionDirectCredit (transactionDirectCreditTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

transactionDirectCreditTests :: TestTree
transactionDirectCreditTests = testGroup "TransactionDirectCredit"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "transaction_direct_credit"
  insertTransactionDirectCredit
  getTransactionDirectCredit
  (\ n _ -> n)
  (TransactionDirectCredit 1 123456)
