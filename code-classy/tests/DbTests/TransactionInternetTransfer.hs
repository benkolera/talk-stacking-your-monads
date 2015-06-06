{-# LANGUAGE OverloadedStrings #-}
module DbTests.TransactionInternetTransfer (transactionInternetTransferTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

transactionInternetTransferTests :: TestTree
transactionInternetTransferTests = testGroup "TransactionInternetTransfer"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "transaction_internet_transfer"
  insertTransactionInternetTransfer
  getTransactionInternetTransfer
  (\ n _ -> n)
  (TransactionInternetTransfer 1 123456 "Xact Ref")
