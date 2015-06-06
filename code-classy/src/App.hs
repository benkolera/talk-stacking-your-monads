{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ConstraintKinds            #-}
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
makeClassy ''AppEnv
data AppError = AppCsvError CsvError | AppDbError DbError
makeClassyPrisms ''AppError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsCsvError AppError where
  _CsvError = _AppCsvError . _CsvError

instance HasDbEnv AppEnv where
  dbEnv = appEnvDb . dbEnv

type CanApp c e m =
  ( CanDb c e m
  , CanCsv e m
  , AsAppError e
  , HasAppEnv c
  )

loadAndInsert :: CanApp c e m => FilePath -> m [Int]
loadAndInsert p = do
  xacts <- readTransactions p
  insertTransactions xacts
