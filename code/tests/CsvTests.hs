{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CsvTests (csvTests) where

import BasePrelude

import Control.Lens
import Control.Monad.Except (runExceptT)
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Csv
import Types

csvTests :: TestTree
csvTests = testGroup "CsvTests"
  [ testCase "IoException"     ioExceptionTest
  , testCase "DecodeErrors"    decodeErrorsTest
  , testCase "DecodeOk"        decodeOkTest
  ]

ioExceptionTest :: Assertion
ioExceptionTest = do
  e <- runExceptT $ readTransactions "idontexisttrolololol"
  e^?_Left._CsvIoError.to show @?= Just "idontexisttrolololol: openBinaryFile: does not exist (No such file or directory)"

decodeErrorsTest :: Assertion
decodeErrorsTest = do
  e <- runExceptT $ readTransactions "tests/csv/broken.csv"
  e @?= Left (CsvDecodeErrors expectedErrors)
  where
    expectedErrors =
      [ "parse error (Failed reading: conversion error: Invalid currency: No Balance) at \"\\r\""
      , "parse error (Failed reading: conversion error: \"VISA PURCHASE   MIEL CONTAINER PREMI BRISBANE CIT 13/02 UK GBP\" (line 1, column 57):\nunexpected \"K\"\nexpecting \"US\") at \"\\r\""
      ]

decodeOkTest :: Assertion
decodeOkTest = do
  e <- runExceptT $ readTransactions "tests/csv/ok.csv"
  e @?= Right expectedTransactions
  where
    expectedTransactions = Transactions "Bank Account" "Everyday Basics" 12345678
      [ Transaction (fromGregorian 2015 2 14) (VisaPurchase (VisaPurchaseDesc (Place "MIEL CONTAINER PREMI BRISBANE CIT") (DdMm 13 2) AU AUD)) (Currency (-40)) (Currency 1684.7)
      , Transaction (fromGregorian 2015 2 14) (AtmOperatorFee (AtmOperatorFeeDesc Withdrawal (Place "Money Machine"))) (Currency (-2.5)) (Currency 1724.7)
      , Transaction (fromGregorian 2015 2 14) (AtmWithdrawal (Place "Money Machine")) (Currency (-20)) (Currency 1727.2)
      , Transaction (fromGregorian 2015 2 14) (VisaPurchase (VisaPurchaseDesc (Place "TeeTurtle") (DdMm 13 2) AU USD)) (Currency (-20)) (Currency 1747.26)
      , Transaction (fromGregorian 2015 2 13) ForeignCurrencyConversionFee (Currency (-1.29)) (Currency 1767.26)
      , Transaction (fromGregorian 2015 2 12) (EftposPurchase "Tenkai Sushi Restaur                  AU") (Currency (-38.5)) (Currency 1768.55)
      , Transaction (fromGregorian 2015 2 11) (EftposPurchase "LB HAIR SALOON PTY L GREENSLOPES QLD") (Currency (-79.95)) (Currency 1807.05)
      , Transaction (fromGregorian 2015 2 10) (DirectCredit (DirectCreditDesc (Place "Magic Pay Place Ref") 654321)) (Currency 1337) (Currency 1887)
      , Transaction (fromGregorian 2015 2 10) (InternetTransferCredit (InternetTransferDesc 12345679 "41514802")) (Currency 50) (Currency 550)
      , Transaction (fromGregorian 2015 2  9) (InternetTransferDebit  (InternetTransferDesc 12345679 "13924810")) (Currency (-200)) (Currency 500)
      ]
