{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App where

import BasePrelude hiding (first)

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans  (MonadIO, liftIO)
import Data.Bifunctor       (first)

import Csv
import Db
import Utils

data AppEnv   = AppEnv { _appEnvDb :: DbEnv }
makeLenses ''AppEnv
data AppError = AppCsvError CsvError | AppDbError DbError
makePrisms ''AppError

newtype App a = App
  { unApp :: (ReaderT AppEnv (ExceptT AppError IO) a)
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader AppEnv
    , MonadIO
    , MonadError AppError
    )

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp e = runExceptT . flip runReaderT e . unApp

loadAndInsert :: FilePath -> App [Int]
loadAndInsert p = do
  xacts <- liftCsv $ readTransactions p
  liftDb $ insertTransactions xacts

liftCsv :: (Applicative m,MonadError AppError m,MonadIO m) => Csv a -> m a
liftCsv c = do
  res <- liftIO $ runCsv c
  throwEither . first AppCsvError $ res

liftDb :: (Applicative m,MonadReader AppEnv m, MonadError AppError m,MonadIO m) => Db a -> m a
liftDb c = do
  e <- view appEnvDb
  res <- liftIO $ runDb e c
  throwEither . first AppDbError $ res
