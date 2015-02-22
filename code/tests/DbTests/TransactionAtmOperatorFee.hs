{-# LANGUAGE OverloadedStrings #-}
module DbTests.TransactionAtmOperatorFee (transactionAtmOperatorFeeTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Db
import DbTests.Internal

transactionAtmOperatorFeeTests :: TestTree
transactionAtmOperatorFeeTests = testGroup "TransactionAtmOperatorFee"
  [ testCase "roundTrip" testRoundTrip ]

testRoundTrip :: Assertion
testRoundTrip = roundTripTest "transaction_atm_operator_fee"
  insertTransactionAtmOperatorFee
  getTransactionAtmOperatorFee
  (\ n _ -> n)
  (TransactionAtmOperatorFee 1 "Withdrawal")
