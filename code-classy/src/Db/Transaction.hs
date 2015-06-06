{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Db.Transaction
  ( Transaction'(Transaction)
  , NewTransaction
  , Transaction
  , transactionQuery
  , allTransactions
  , getTransaction
  , insertTransaction
  , transactionId
  , transactionDate
  , transactionAmount
  , transactionBalance
  , transactionType
  , transactionPlaceId
  , transactionAccountId
  ) where

import BasePrelude hiding (optional)

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Data.Time                  (Day)
import Opaleye

import Db.Internal

data Transaction' a b c d e f g = Transaction
  { _transactionId        :: a
  , _transactionDate      :: b
  , _transactionAmount    :: c
  , _transactionBalance   :: d
  , _transactionType      :: e
  , _transactionPlaceId   :: f
  , _transactionAccountId :: g
  } deriving (Eq,Show)
makeLenses ''Transaction'

type Transaction = Transaction' Int Day Double Double Text (Maybe Int) Int
type TransactionColumn = Transaction'
  (Column PGInt4)
  (Column PGDate)
  (Column PGFloat8) -- These should be non-floating point numbers
  (Column PGFloat8) -- but Opaleye doesn't support these yet. :(
  (Column PGText)
  (Column (Nullable PGInt4))
  (Column PGInt4)

makeAdaptorAndInstance "pTransaction" ''Transaction'

type NewTransaction = Transaction' (Maybe Int) Day Double Double Text (Maybe Int) Int

type NewTransactionColumn = Transaction'
  (Maybe (Column PGInt4))
  (Column PGDate)
  (Column PGFloat8)
  (Column PGFloat8)
  (Column PGText)
  (Column (Nullable PGInt4))
  (Column PGInt4)

transactionTable :: Table NewTransactionColumn TransactionColumn
transactionTable = Table "transaction" $ pTransaction Transaction
  { _transactionId        = optional "id"
  , _transactionDate      = required "date"
  , _transactionAmount    = required "amount"
  , _transactionBalance   = required "balance"
  , _transactionType      = required "type"
  , _transactionPlaceId   = required "place_id"
  , _transactionAccountId = required "account_id"
  }

transactionQuery :: Query TransactionColumn
transactionQuery = queryTable transactionTable

allTransactions :: CanDb c e m => m [Transaction]
allTransactions = liftQuery transactionQuery

insertTransaction :: CanDb c e m => NewTransaction -> m Int
insertTransaction =
  liftInsertReturningFirst transactionTable (view transactionId) . packNew

getTransaction :: CanDb c e m => Int -> m (Maybe Transaction)
getTransaction i = liftQueryFirst $ proc () -> do
  t <- transactionQuery -< ()
  restrict -< t^.transactionId .== pgInt4 i
  returnA  -< t

packNew :: NewTransaction -> NewTransactionColumn
packNew = pTransaction Transaction
  { _transactionId        = fmap pgInt4
  , _transactionDate      = pgDay
  , _transactionAmount    = pgDouble
  , _transactionBalance   = pgDouble
  , _transactionType      = pgStrictText
  , _transactionPlaceId   = maybeToNullable . fmap pgInt4
  , _transactionAccountId = pgInt4
  }
