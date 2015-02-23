{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Db.TransactionAtmOperatorFee
  ( TransactionAtmOperatorFee'(TransactionAtmOperatorFee)
  , NewTransactionAtmOperatorFee
  , TransactionAtmOperatorFee
  , transactionAtmOperatorFeeQuery
  , allTransactionAtmOperatorFees
  , getTransactionAtmOperatorFee
  , insertTransactionAtmOperatorFee
  , transactionAtmOperatorFeeTransactionId
  , transactionAtmOperatorFeeTransactionType
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Opaleye

import Db.Internal

data TransactionAtmOperatorFee' a b = TransactionAtmOperatorFee
  { _transactionAtmOperatorFeeTransactionId   :: a
  , _transactionAtmOperatorFeeTransactionType :: b
  } deriving (Eq,Show)
makeLenses ''TransactionAtmOperatorFee'

type TransactionAtmOperatorFee = TransactionAtmOperatorFee' Int Text
type TransactionAtmOperatorFeeColumn = TransactionAtmOperatorFee'
  (Column PGInt4)
  (Column PGText)

makeAdaptorAndInstance "pTransactionAtmOperatorFee" ''TransactionAtmOperatorFee'

type NewTransactionAtmOperatorFee = TransactionAtmOperatorFee' Int Text

type NewTransactionAtmOperatorFeeColumn = TransactionAtmOperatorFee'
  (Column PGInt4)
  (Column PGText)

transactionAtmOperatorFeeTable :: Table NewTransactionAtmOperatorFeeColumn TransactionAtmOperatorFeeColumn
transactionAtmOperatorFeeTable = Table "transaction_atm_operator_fee" $ pTransactionAtmOperatorFee TransactionAtmOperatorFee
  { _transactionAtmOperatorFeeTransactionId    = required "transaction_id"
  , _transactionAtmOperatorFeeTransactionType  = required "atm_transaction_type"
  }

transactionAtmOperatorFeeQuery :: Query TransactionAtmOperatorFeeColumn
transactionAtmOperatorFeeQuery = queryTable transactionAtmOperatorFeeTable

allTransactionAtmOperatorFees :: Db [TransactionAtmOperatorFee]
allTransactionAtmOperatorFees = liftQuery transactionAtmOperatorFeeQuery

getTransactionAtmOperatorFee :: Int -> Db (Maybe TransactionAtmOperatorFee)
getTransactionAtmOperatorFee i = liftQueryFirst $ proc () -> do
  tc <- transactionAtmOperatorFeeQuery -< ()
  restrict -< tc^.transactionAtmOperatorFeeTransactionId .== pgInt4 i
  returnA -< tc

insertTransactionAtmOperatorFee :: NewTransactionAtmOperatorFee -> Db Int
insertTransactionAtmOperatorFee =
  liftInsertReturningFirst transactionAtmOperatorFeeTable (view transactionAtmOperatorFeeTransactionId)
  . packNew

packNew :: NewTransactionAtmOperatorFee -> NewTransactionAtmOperatorFeeColumn
packNew = pTransactionAtmOperatorFee TransactionAtmOperatorFee
  { _transactionAtmOperatorFeeTransactionId    = pgInt4
  , _transactionAtmOperatorFeeTransactionType  = pgStrictText
  }
