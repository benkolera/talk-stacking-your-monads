{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Db.TransactionVisa
  ( TransactionVisa'(TransactionVisa)
  , NewTransactionVisa
  , TransactionVisa
  , transactionVisaQuery
  , getTransactionVisa
  , insertTransactionVisa
  , transactionVisaTransactionId
  , transactionVisaPurchaseDate
  , transactionVisaCurrencyCode
  , transactionVisaCountryCode
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Data.Time                  (Day)
import Opaleye

import Db.Internal

data TransactionVisa' a b c d = TransactionVisa
  { _transactionVisaTransactionId :: a
  , _transactionVisaPurchaseDate  :: b
  , _transactionVisaCurrencyCode  :: c
  , _transactionVisaCountryCode   :: d
  } deriving (Eq,Show)
makeLenses ''TransactionVisa'

type TransactionVisa = TransactionVisa' Int Day Text Text
type TransactionVisaColumn = TransactionVisa'
  (Column PGInt4)
  (Column PGDate)
  (Column PGText)
  (Column PGText)

makeAdaptorAndInstance "pTransactionVisa" ''TransactionVisa'

type NewTransactionVisa = TransactionVisa' Int Day Text Text

type NewTransactionVisaColumn = TransactionVisa'
  (Column PGInt4)
  (Column PGDate)
  (Column PGText)
  (Column PGText)

transactionVisaTable :: Table NewTransactionVisaColumn TransactionVisaColumn
transactionVisaTable = Table "transaction_visa" $ pTransactionVisa TransactionVisa
  { _transactionVisaTransactionId = required "transaction_id"
  , _transactionVisaPurchaseDate  = required "purchase_date"
  , _transactionVisaCurrencyCode  = required "currency_code"
  , _transactionVisaCountryCode   = required "country_code"
  }

transactionVisaQuery :: Query TransactionVisaColumn
transactionVisaQuery = queryTable transactionVisaTable

getTransactionVisa :: Int -> Db (Maybe TransactionVisa)
getTransactionVisa i = liftQueryFirst $ proc () -> do
  tc <- transactionVisaQuery -< ()
  restrict -< tc^.transactionVisaTransactionId .== pgInt4 i
  returnA -< tc

insertTransactionVisa :: NewTransactionVisa -> Db Int
insertTransactionVisa =
  liftInsertReturningFirst transactionVisaTable (view transactionVisaTransactionId)
  . packNew

packNew :: NewTransactionVisa -> NewTransactionVisaColumn
packNew = pTransactionVisa TransactionVisa
  { _transactionVisaTransactionId = pgInt4
  , _transactionVisaPurchaseDate  = pgDay
  , _transactionVisaCurrencyCode  = pgStrictText
  , _transactionVisaCountryCode   = pgStrictText
  }
