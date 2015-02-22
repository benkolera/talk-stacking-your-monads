{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Types where

import BasePrelude

import Control.Lens
import Data.Text    (Text)
import Data.Time    (Day)

newtype Place = Place Text deriving (Eq,Show)
makeWrapped ''Place

newtype Currency = Currency Double deriving (Eq,Show)
makeWrapped ''Currency

data DdMm = DdMm
  { ddMmDay   :: Integer
  , ddMmMonth :: Integer
  } deriving (Eq,Show)
makeLenses ''DdMm

data CountryCode = AU | US deriving (Eq,Show)
makePrisms ''CountryCode

data CurrencyCode = AUD | USD deriving (Eq,Show)
makePrisms ''CurrencyCode

data VisaPurchaseDesc = VisaPurchaseDesc
  { _visaPurchasePlace    :: Place
  , _visaPurchaseDate     :: DdMm
  , _visaPurchaseCountry  :: CountryCode
  , _visaPurchaseCurrency :: CurrencyCode
  } deriving (Eq,Show)
makeLenses ''VisaPurchaseDesc

data AtmOperatorFeeType = Withdrawal deriving (Eq,Show)
makePrisms ''AtmOperatorFeeType

data AtmOperatorFeeDesc = AtmOperatorFeeDesc
  { _atmOperatorFeeType  :: AtmOperatorFeeType
  , _atmOperatorFeePlace :: Place
  } deriving (Eq,Show)
makeLenses ''AtmOperatorFeeDesc

data DirectCreditDesc = DirectCreditDesc
  { _directCreditPlace :: Place
  , _directCreditBsb   :: Integer
  } deriving (Eq,Show)
makeLenses ''DirectCreditDesc

data InternetTransferDesc = InternetTransferDesc
  { _internetTransferAccount :: Integer
  , _internetTransferRef     :: Text
  } deriving (Eq,Show)
makeLenses ''InternetTransferDesc

data TransactionDesc
  = VisaPurchase VisaPurchaseDesc
  | EftposPurchase Place
  | ForeignCurrencyConversionFee
  | AtmOperatorFee AtmOperatorFeeDesc
  | AtmWithdrawal Place
  | DirectCredit DirectCreditDesc
  | InternetTransferCredit InternetTransferDesc
  | InternetTransferDebit  InternetTransferDesc
  deriving (Eq,Show)
makePrisms ''TransactionDesc

-- transactionDescPlace :: Getter TransactionDesc (Maybe Place)
transactionDescPlace :: Traversal' TransactionDesc Place
transactionDescPlace =
  _VisaPurchase.visaPurchasePlace
  `failing` _EftposPurchase
  `failing` _AtmWithdrawal

-- transactionDescPlace' :: TransactionDesc -> Maybe Place
-- transactionDescPlace' (VisaPurchase v)   = Just $ v^.visaPurchasePlace
-- transactionDescPlace' (EftposPurchase p) = Just p
-- transactionDescPlace' (AtmWithdrawal p)  = Just p
-- transactionDescPlace' _                  = Nothing

data Transaction = Transaction
  { _transactionDate    :: Day
  , _transactionDesc    :: TransactionDesc
  , _transactionAmount  :: Currency
  , _transactionBalance :: Currency
  } deriving (Eq,Show)
makeLenses ''Transaction

data Transactions = Transactions
  { _transactionsAcctName :: Text
  , _transactionsAcctType :: Text
  , _transactionsAcctNum  :: Int
  , _transactions         :: [Transaction]
  } deriving (Eq,Show)
makeLenses ''Transactions
