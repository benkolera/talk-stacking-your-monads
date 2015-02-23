{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DbTests (dbTests) where

import BasePrelude

import Data.Time        (fromGregorian)
import Test.Tasty
import Test.Tasty.HUnit

import           Db
import           DbTests.Account
import           DbTests.Internal
import           DbTests.Place
import           DbTests.PlaceCategory
import           DbTests.Transaction
import           DbTests.TransactionAtmOperatorFee
import           DbTests.TransactionDirectCredit
import           DbTests.TransactionInternetTransfer
import           DbTests.TransactionVisa
import qualified Types                               as T

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
  , testCase "insertTransactions" insertTransactionsTest
  , testGroup "setYear"
    [ testCase "sameMonth" setYearSameMonthTest
    , testCase "sameYear"  setYearSameYearTest
    , testCase "sameDay"   setYearSameDayTest
    , testCase "prevYear"  setYearPrevYearTest
    ]
  ]

insertTransactionsTest :: Assertion
insertTransactionsTest = withDb "insert_transactions" $ \ e -> do
  res <- runDb e $ do
    void $ insertTransactions (T.Transactions "Savings Account" "Savings" 12345678
      [ T.Transaction (fromGregorian 2015 2 23) T.ForeignCurrencyConversionFee (-0.22) 99.88
      , T.Transaction (fromGregorian 2015 2 23) (T.VisaPurchase (T.VisaPurchaseDesc "TeeTurtle" (T.DdMm 22 2) T.AU T.USD)) (-0.22) 99.88
      , T.Transaction (fromGregorian 2015 2 23) (T.EftposPurchase "Tenkai Sushi") (-30.88) 69.0
      , T.Transaction (fromGregorian 2015 2 23) (T.AtmOperatorFee (T.AtmOperatorFeeDesc T.Withdrawal "Platform Bar")) (-2.5) 66.5
      , T.Transaction (fromGregorian 2015 2 23) (T.AtmWithdrawal "Platform Bar") (-20) 46.50
      , T.Transaction (fromGregorian 2015 2 23) (T.DirectCredit (T.DirectCreditDesc "$WORK" 123456)) 200 246.5
      , T.Transaction (fromGregorian 2015 2 23) (T.InternetTransferCredit (T.InternetTransferDesc 12345679 "REF1337")) 400 646.5
      , T.Transaction (fromGregorian 2015 2 23) (T.InternetTransferDebit (T.InternetTransferDesc 12345679 "REF31337")) (-146.5) 500
      ])
    void $ insertTransactions (T.Transactions "Other Account" "Other" 12345679
      [ T.Transaction (fromGregorian 2015 2 23) (T.InternetTransferDebit (T.InternetTransferDesc 12345678 "REF1337")) (-400) 0
      , T.Transaction (fromGregorian 2015 2 23) (T.InternetTransferCredit (T.InternetTransferDesc 12345678 "REF31337")) 146.5 146.5
      ])
    ps   <- allPlaces
    ts   <- allTransactions
    aofs <- allTransactionAtmOperatorFees
    vs   <- allTransactionVisas
    dcs  <- allTransactionDirectCredits
    its  <- allTransactionInternetTransfers
    pure (ps,ts,aofs,vs,dcs,its)
  assertDbResult res $ \ (ps,ts,aofs,vs,dcs,its) -> do
    ps @?=
      [ Place 1 "TeeTurtle" Nothing
      , Place 2 "Tenkai Sushi" Nothing
      , Place 3 "Platform Bar" Nothing
      , Place 4 "$WORK" Nothing
      ]
    ts @?=
      [ Transaction 1 (fromGregorian 2015 2 23) (-0.22) 99.88 "foreign_currency_conversion_fee" Nothing 1
      , Transaction 2 (fromGregorian 2015 2 23) (-0.22) 99.88 "visa" (Just 1) 1
      , Transaction 3 (fromGregorian 2015 2 23) (-30.88) 69 "eftpos" (Just 2) 1
      , Transaction 4 (fromGregorian 2015 2 23) (-2.5) 66.5 "atm_operator_fee" (Just 3) 1
      , Transaction 5 (fromGregorian 2015 2 23) (-20) 46.5 "atm_withdrawal" (Just 3) 1
      , Transaction 6 (fromGregorian 2015 2 23) 200 246.5 "direct_credit" (Just 4) 1
      , Transaction 7 (fromGregorian 2015 2 23) 400 646.5 "internet_transfer_credit" Nothing 1
      , Transaction 8 (fromGregorian 2015 2 23) (-146.5) 500 "internet_transfer_debit" Nothing 1
      , Transaction 9 (fromGregorian 2015 2 23) (-400) 0 "internet_transfer_debit" Nothing 2
      , Transaction 10 (fromGregorian 2015 2 23) 146.5 146.5 "internet_transfer_credit" Nothing 2
      ]
    aofs @?= [ TransactionAtmOperatorFee 4 "withdrawal" ]
    vs   @?= [ TransactionVisa 2 (fromGregorian 2015 2 22) "USD" "AU" ]
    dcs  @?= [ TransactionDirectCredit 6 123456 ]
    its  @?=
      [ TransactionInternetTransfer 7 12345679 "REF1337"
      , TransactionInternetTransfer 8 12345679 "REF31337"
      , TransactionInternetTransfer 9 12345678 "REF1337"
      , TransactionInternetTransfer 10 12345678 "REF31337"
      ]

setYearSameDayTest :: Assertion
setYearSameDayTest =
  (setYear (fromGregorian 2015 02 23) (T.DdMm 23 2)) @?= fromGregorian 2015 2 23

setYearSameMonthTest :: Assertion
setYearSameMonthTest =
  (setYear (fromGregorian 2015 02 23) (T.DdMm 10 2)) @?= fromGregorian 2015 2 10

setYearSameYearTest :: Assertion
setYearSameYearTest =
  (setYear (fromGregorian 2015 02 23) (T.DdMm 23 1)) @?= fromGregorian 2015 1 23

setYearPrevYearTest :: Assertion
setYearPrevYearTest =
  (setYear (fromGregorian 2015 02 23) (T.DdMm 23 12)) @?= fromGregorian 2014 12 23
