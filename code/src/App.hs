{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module App where

import BasePrelude hiding (first)

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans  (MonadIO, liftIO)
import Data.Bifunctor       (first)

import Csv
import Db
import Utils

data AppEnv   = AppEnv { _appEnvDb :: DbEnv }
data AppError = AppCsvError CsvError

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

runCsv :: (Applicative m,MonadError AppError m,MonadIO m) => ExceptT CsvError IO a -> m a
runCsv c = do
  res <- liftIO $ runExceptT c
  throwEither . first AppCsvError $ res
