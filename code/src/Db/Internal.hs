{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Db.Internal where

import BasePrelude

import Control.Error                   (headMay)
import Control.Lens                    (makeLenses, makePrisms, view)
import Control.Monad.Except            (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader            (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans             (MonadIO)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (Connection, QueryError, SqlError, close)
import Opaleye                         (Column, PGBool, Query, QueryRunner,
                                        Table, Unpackspec, runDelete, runInsert,
                                        runInsertReturning, runQuery, runUpdate)

import Utils (wrapExceptions)

data DbError
  = DbQueryError QueryError
  | DbSqlError SqlError
  deriving (Show)
makePrisms ''DbError

data DbEnv = DbEnv
  { _dbEnvConnection :: Connection
  }
makeLenses ''DbEnv

newtype Db a = Db
  { unDb :: ExceptT DbError (ReaderT DbEnv IO) a
  } deriving
   ( Functor
   , Applicative
   , Monad
   , MonadReader DbEnv
   , MonadError DbError
   , MonadIO
   )

runDb :: DbEnv -> Db a -> IO (Either DbError a)
runDb e = flip runReaderT e . runExceptT . unDb

closeDbEnv :: DbEnv -> IO ()
closeDbEnv = close . view dbEnvConnection

liftQuery
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    , Default QueryRunner columnsW haskells
    )
  => Query columnsW
  -> m [haskells]
liftQuery q = withConnection (`runQuery` q)

liftQueryFirst
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Functor m
    , Applicative m
    , MonadIO m
    , Default QueryRunner columnsW haskells
    )
  => Query columnsW
  -> m (Maybe haskells)
liftQueryFirst = fmap headMay . liftQuery

liftInsert
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    )
  => Table columnsW columnsR
  -> columnsW
  -> m Int64
liftInsert t c = withConnection (\ con -> runInsert con t c)

liftInsertReturning
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    , Default QueryRunner returned haskells
    , Default Unpackspec returned returned
    )
  => Table columnsW columnsR
  -> (columnsR -> returned)
  -> columnsW
  -> m [haskells]
liftInsertReturning t f c = withConnection (\ con -> runInsertReturning con t c f)

liftUpdate
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    )
  => Table columnsW columnsR
  -> (columnsR -> columnsW)
  -> (columnsR -> Column PGBool)
  -> m Int64
liftUpdate t f w = withConnection (\ con -> runUpdate con t f w)

liftDelete
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    )
  => Table columnsW columnsR
  -> (columnsR -> Column PGBool)
  -> m Int64
liftDelete t w = withConnection (\ con -> runDelete con t w)

withConnection
  :: ( MonadReader DbEnv m
    , MonadError DbError m
    , Applicative m
    , MonadIO m
    )
  => (Connection -> IO a)
  -> m a
withConnection f = do
  c <- view dbEnvConnection
  wrapExceptions (f c)
    [ Handler (pure . DbSqlError)
    , Handler (pure . DbQueryError)
    ]
