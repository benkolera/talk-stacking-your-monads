{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ) where

import BasePrelude

import Control.Lens
import Data.Text    (Text)

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
      insertTransaction $ Transaction
        Nothing
        (xact^. T.transactionDate)
        (xact^. T.transactionAmount._Wrapped)
        (xact^. T.transactionBalance._Wrapped)
        (xact^. T.transactionDesc.to transactionDescToDbType)
        pIdMay
        acctId
    upsertPlaceByName' n = upsertPlaceByName (Place Nothing (n^._Wrapped) Nothing)
    xactAcct = Account
      Nothing
      (xacts^. T.transactionsAcctType)
      (xacts^. T.transactionsAcctNum)
      (xacts^. T.transactionsAcctName)

transactionDescToDbType :: T.TransactionDesc -> Text
transactionDescToDbType (T.AtmOperatorFee _)           = "atm_operator_fee"
transactionDescToDbType (T.AtmWithdrawal _)            = "atm_withdrawal"
transactionDescToDbType (T.DirectCredit _)             = "direct_credit"
transactionDescToDbType (T.InternetTransferCredit _)   = "internet_transfer_credit"
transactionDescToDbType (T.InternetTransferDebit _)   = "internet_transfer_debit"
transactionDescToDbType (T.VisaPurchase _)             = "visa"
transactionDescToDbType (T.EftposPurchase _)           = "eftpos"
transactionDescToDbType T.ForeignCurrencyConversionFee = "foreign_currency_conversion_fee"
