{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Db.TransactionDirectCredit
  ( TransactionDirectCredit'(TransactionDirectCredit)
  , NewTransactionDirectCredit
  , TransactionDirectCredit
  , transactionDirectCreditQuery
  , getTransactionDirectCredit
  , insertTransactionDirectCredit
  , transactionDirectCreditTransactionId
  , transactionDirectCreditBsb
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye

import Db.Internal

data TransactionDirectCredit' a b = TransactionDirectCredit
  { _transactionDirectCreditTransactionId :: a
  , _transactionDirectCreditBsb           :: b
  } deriving (Eq,Show)
makeLenses ''TransactionDirectCredit'

type TransactionDirectCredit = TransactionDirectCredit' Int Int
type TransactionDirectCreditColumn = TransactionDirectCredit'
  (Column PGInt4)
  (Column PGInt4)

makeAdaptorAndInstance "pTransactionDirectCredit" ''TransactionDirectCredit'

type NewTransactionDirectCredit = TransactionDirectCredit' Int Int

type NewTransactionDirectCreditColumn = TransactionDirectCredit'
  (Column PGInt4)
  (Column PGInt4)

transactionDirectCreditTable :: Table NewTransactionDirectCreditColumn TransactionDirectCreditColumn
transactionDirectCreditTable = Table "transaction_direct_credit" $ pTransactionDirectCredit TransactionDirectCredit
  { _transactionDirectCreditTransactionId = required "transaction_id"
  , _transactionDirectCreditBsb           = required "bsb"
  }

transactionDirectCreditQuery :: Query TransactionDirectCreditColumn
transactionDirectCreditQuery = queryTable transactionDirectCreditTable

getTransactionDirectCredit :: Int -> Db (Maybe TransactionDirectCredit)
getTransactionDirectCredit i = liftQueryFirst $ proc () -> do
  tc <- transactionDirectCreditQuery -< ()
  restrict -< tc^.transactionDirectCreditTransactionId .== pgInt4 i
  returnA -< tc

insertTransactionDirectCredit :: NewTransactionDirectCredit -> Db Int
insertTransactionDirectCredit =
  liftInsertReturningFirst transactionDirectCreditTable (view transactionDirectCreditTransactionId)
  . packNew

packNew :: NewTransactionDirectCredit -> NewTransactionDirectCreditColumn
packNew = pTransactionDirectCredit TransactionDirectCredit
  { _transactionDirectCreditTransactionId = pgInt4
  , _transactionDirectCreditBsb           = pgInt4
  }
