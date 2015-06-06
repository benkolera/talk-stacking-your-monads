{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Csv
  ( Csv
  , CsvError(..)
  , _CsvIoError
  , _CsvHeaderParseError
  , _CsvDecodeErrors
  , runCsv
  , readTransactions
  ) where

import BasePrelude hiding (first, try, words)

import           Control.Error              (headMay, note)
import           Control.Lens               (makePrisms, (^.))
import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.TM           ((.>>=.))
import           Control.Monad.Trans        (MonadIO)
import           Data.Bifunctor             (bimap, first)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Csv                   (FromField (..), FromRecord (..),
                                             HasHeader (NoHeader), decode, (.!))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Time                  (Day, parseTimeM,defaultTimeLocale)
import           Data.Validation            (AccValidation, _AccValidation)
import qualified Data.Vector                as V
import           Text.Parsec                (alphaNum, anyChar, char, choice,
                                             digit, lookAhead, many1, manyTill,
                                             parse, sepEndBy1, space, spaces,
                                             string, try)
import           Text.Parsec.Text           (Parser)

import Types hiding (transactionDesc)
import Utils

data CsvError
  = CsvIoError IOException
  | CsvHeaderParseError String
  | CsvDecodeErrors [String]
  deriving (Eq,Show)
makePrisms ''CsvError

newtype Csv a = Csv { unCsv :: ExceptT CsvError IO a }
  deriving (Functor,Applicative,Monad,MonadError CsvError,MonadIO)

runCsv :: Csv a -> IO (Either CsvError a)
runCsv = runExceptT . unCsv

readTransactions :: FilePath -> Csv Transactions
readTransactions fn = do
  lbs <- wrapException CsvIoError $ (LBS.readFile fn)
  let (headers,csvs) = splitAt 2 . LC8.lines $ lbs
  (name,t,num) <- parseHeader headers
  xactsV <- throwAccValidation CsvDecodeErrors $ csvs .>>=. decodeCsvLine -- t .>>=. f is fmap fold . traverse f t
  pure $ Transactions name t num xactsV

decodeCsvLine
  :: FromRecord a
  => LBS.ByteString
  -> AccValidation [String] [a]
decodeCsvLine bs =
  (bimap (:[]) toList . decode NoHeader $ bs) ^._AccValidation

parseHeader
  :: (MonadError CsvError m, Applicative m)
  => [LBS.ByteString]
  -> m (T.Text,T.Text,Int)
parseHeader hs = throwEither . first CsvHeaderParseError $ do
  h <- note "Header Missing" . headMay $ hs
  first show $ parse header (LC8.unpack h) (decodeUtf8 . LBS.toStrict $ h)
  where
    header = do
      void $ string "\"Account History for Account:\",\""
      name <- manyTill anyChar (try $ string " - ")
      t    <- manyTill anyChar (try $ string " - ")
      num  <- int
      pure (T.pack name,T.pack t,num)

words :: Parser T.Text
words = T.unwords . fmap T.pack <$> sepEndBy1 (many1 alphaNum) space

int :: Parser Int
int = read <$> many1 digit

anyText :: Parser T.Text
anyText = T.pack <$> many anyChar

enumParser :: a -> String -> Parser a
enumParser c s = const c <$> string s

desc :: String -> Parser a -> Parser a
desc s p = string s *> spaces *> p

transactionDesc :: Parser TransactionDesc
transactionDesc = choice
  [ desc "VISA PURCHASE"                   $ fmap VisaPurchase visaPurchaseDesc
  , desc "EFTPOS WDL"                      $ fmap (EftposPurchase . Place) anyText
  , desc "FOREIGN CURRENCY CONVERSION FEE" $ pure ForeignCurrencyConversionFee
  , desc "DIRECT CREDIT"                   $ fmap DirectCredit directCreditDesc
  , string "ATM " *> choice
    [ desc "OPERATOR FEE" $ fmap AtmOperatorFee atmOperatorFeeDesc
    , desc "WITHDRAWAL"   $ fmap AtmWithdrawal place
    ]
  , string "INTERNET TRANSFER " *> choice
    [ desc "CREDIT FROM"   $ fmap InternetTransferCredit (internetTransferDesc "REF NO")
    , desc "DEBIT TO"      $ fmap InternetTransferDebit (internetTransferDesc "REFERENCE NO")
    ]
  ]

visaPurchaseDesc :: Parser VisaPurchaseDesc
visaPurchaseDesc = VisaPurchaseDesc
  -- This one is different because we can't rely on the double space to end things
  -- So lets parse anyChar until we hit space then the DD/MM bit
  <$> (Place . T.pack <$> manyTill anyChar (try . lookAhead $ space >> ddMm))
  <*> (spaces *> ddMm)
  <*> (spaces *> countryCode)
  <*> (spaces *> currencyCode)

directCreditDesc :: Parser DirectCreditDesc
directCreditDesc = DirectCreditDesc
  <$> place
  <*> (spaces *> int)

place :: Parser Place
place = Place <$> words

atmOperatorFeeDesc :: Parser AtmOperatorFeeDesc
atmOperatorFeeDesc = AtmOperatorFeeDesc
  <$> choice [enumParser Withdrawal "WITHDRAWAL"]
  <*> (spaces *> place)

internetTransferDesc :: String -> Parser InternetTransferDesc
internetTransferDesc s = InternetTransferDesc
  <$> int
  <*> (spaces *> string s *> spaces *> anyText)

countryCode :: Parser CountryCode
countryCode = choice
  [ enumParser AU "AU"
  , enumParser US "US"
  ]

currencyCode :: Parser CurrencyCode
currencyCode = choice
  [ enumParser AUD "AUD"
  , enumParser USD "USD"
  ]

ddMm :: Parser DdMm
ddMm = DdMm <$> (int <* char '/') <*> int

instance FromField Currency where
  parseField bs =
    let s    = C8.unpack bs
        sign = bool id (0-) $ isPrefixOf "-" s
    in maybe (fail $ "Invalid currency: " <> s) (pure . Currency . sign)
       . readMaybe
       . filter (/= ',')
       . dropWhile (== '$')
       . dropWhile (== '-')
       $ s

instance FromField TransactionDesc where
  parseField s = either (fail . show) pure $
    parse transactionDesc (C8.unpack s) (decodeUtf8 s)

instance FromField Day where
  parseField s = maybe (fail "Invalid DD/MM/YYYY date") pure $
    parseTimeM True defaultTimeLocale "%d/%m/%Y" (C8.unpack s)

instance FromRecord Transaction where
  parseRecord v
    | V.length v == 4 = Transaction <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
    | otherwise      = fail $ "Invalid number of fields: " <> show (V.length v)
