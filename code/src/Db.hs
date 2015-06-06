{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Db
  ( module Db.Account
  , module Db.Internal
  , module Db.Place
  , module Db.PlaceCategory
  , module Db.Transaction
  , module Db.TransactionDirectCredit
  , module Db.TransactionAtmOperatorFee
  , module Db.TransactionInternetTransfer
  , module Db.TransactionVisa
  , insertTransactions
  , setYear
  ) where

import BasePrelude

import Control.Lens
import Data.Text    (Text, pack)
import Data.Time

import           Db.Account
import           Db.Internal                    (Db, DbEnv (..), DbError (..),
                                                 closeDbEnv, dbEnvConnection,
                                                 liftQuery, runDb,
                                                 _DbQueryError, _DbSqlError)
import           Db.Place
import           Db.PlaceCategory
import           Db.Transaction
import           Db.TransactionAtmOperatorFee
import           Db.TransactionDirectCredit
import           Db.TransactionInternetTransfer
import           Db.TransactionVisa
import qualified Types                          as T

insertTransactions :: T.Transactions -> Db [Int]
insertTransactions xacts = do
  a <- upsertAccountByNumber xactAcct
  traverse (insertTransaction' a) $ xacts^. T.transactions
  where
    insertTransaction' acctId xact = do
      pIdMay <- traverse upsertPlaceByName' (xact^?T.transactionDesc.T.transactionDescPlace)
      i <- insertTransaction $ Transaction
        Nothing
        (xact^. T.transactionDate)
        (xact^. T.transactionAmount._Wrapped)
        (xact^. T.transactionBalance._Wrapped)
        (xact^. T.transactionDesc.to transactionDescToDbType)
        pIdMay
        acctId
      traverse_ (insertTransactionVisa' i (xact^.T.transactionDate)) $ xact ^? T.transactionDesc.T._VisaPurchase
      traverse_ (insertTransactionAtmOperatorFee' i) $ xact ^? T.transactionDesc.T._AtmOperatorFee
      traverse_ (insertTransactionDirectCredit' i) $ xact ^? T.transactionDesc.T._DirectCredit
      traverse_ (insertTransactionInternetTransfer' i) $ xact ^? T.transactionDesc.T._InternetTransferCredit
      traverse_ (insertTransactionInternetTransfer' i) $ xact ^? T.transactionDesc.T._InternetTransferDebit
      pure i
    upsertPlaceByName' n = upsertPlaceByName (Place Nothing (n^._Wrapped) Nothing)
    insertTransactionVisa' i dt v = insertTransactionVisa $
      TransactionVisa i
        (setYear dt (v^.T.visaPurchaseDate))
        (v^.T.visaPurchaseCurrency.to (pack . show))
        (v^.T.visaPurchaseCountry.to (pack . show))

    insertTransactionDirectCredit' i dc = insertTransactionDirectCredit $
      TransactionDirectCredit i (dc^.T.directCreditBsb)

    insertTransactionAtmOperatorFee' i aof = insertTransactionAtmOperatorFee $
      TransactionAtmOperatorFee i (aof^.T.atmOperatorFeeType.to operatorFeeTypeToDb)

    insertTransactionInternetTransfer' i it = insertTransactionInternetTransfer $
      TransactionInternetTransfer i
        (it^. T.internetTransferAccount)
        (it^. T.internetTransferRef)

    xactAcct = Account
      Nothing
      (xacts^. T.transactionsAcctType)
      (xacts^. T.transactionsAcctNum)
      (xacts^. T.transactionsAcctName)

setYear :: Day -> T.DdMm -> Day
setYear dt (T.DdMm dd mm) =
  let (y,m,d) = toGregorian dt
  in fromGregorian (bool y (y -1) (dd > d || mm > m)) mm dd

operatorFeeTypeToDb :: T.AtmOperatorFeeType -> Text
operatorFeeTypeToDb T.Withdrawal = "withdrawal"

transactionDescToDbType :: T.TransactionDesc -> Text
transactionDescToDbType (T.AtmOperatorFee _)           = "atm_operator_fee"
transactionDescToDbType (T.AtmWithdrawal _)            = "atm_withdrawal"
transactionDescToDbType (T.DirectCredit _)             = "direct_credit"
transactionDescToDbType (T.InternetTransferCredit _)   = "internet_transfer_credit"
transactionDescToDbType (T.InternetTransferDebit _)   = "internet_transfer_debit"
transactionDescToDbType (T.VisaPurchase _)             = "visa"
transactionDescToDbType (T.EftposPurchase _)           = "eftpos"
transactionDescToDbType T.ForeignCurrencyConversionFee = "foreign_currency_conversion_fee"
