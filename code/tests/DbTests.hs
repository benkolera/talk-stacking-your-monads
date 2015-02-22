{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DbTests (dbTests) where

import Test.Tasty

import DbTests.Account
import DbTests.Place
import DbTests.PlaceCategory
import DbTests.Transaction
import DbTests.TransactionAtmOperatorFee
import DbTests.TransactionDirectCredit
import DbTests.TransactionInternetTransfer
import DbTests.TransactionVisa

dbTests :: TestTree
dbTests = testGroup "DbTests"
  [ accountTests
  , placeTests
  , placeCategoryTests
  , transactionTests
  , transactionAtmOperatorFeeTests
  , transactionDirectCreditTests
  , transactionInternetTransferTests
  , transactionVisaTests
  ]
