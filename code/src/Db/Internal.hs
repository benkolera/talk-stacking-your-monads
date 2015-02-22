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
  :: ( Default QueryRunner columnsW haskells )
  => Query columnsW
  -> Db [haskells]
liftQuery q = withConnection (`runQuery` q)

liftQueryFirst
  :: ( Default QueryRunner columnsW haskells )
  => Query columnsW
  -> Db (Maybe haskells)
liftQueryFirst = fmap headMay . liftQuery

liftInsert
  :: Table columnsW columnsR
  -> columnsW
  -> Db Int64
liftInsert t c = withConnection (\ con -> runInsert con t c)

liftInsertReturning
  :: ( Default QueryRunner returned haskells
    , Default Unpackspec returned returned
    )
  => Table columnsW columnsR
  -> (columnsR -> returned)
  -> columnsW
  -> Db [haskells]
liftInsertReturning t f c = withConnection (\ con -> runInsertReturning con t c f)

liftInsertReturningFirst
  :: ( Default QueryRunner returned haskells
    , Default Unpackspec returned returned
    )
  => Table columnsW columnsR
  -> (columnsR -> returned)
  -> columnsW
  -> Db haskells
liftInsertReturningFirst t f = fmap head . liftInsertReturning t f

liftUpdate
  :: Table columnsW columnsR
  -> (columnsR -> columnsW)
  -> (columnsR -> Column PGBool)
  -> Db Int64
liftUpdate t f w = withConnection (\ con -> runUpdate con t f w)

liftDelete
  :: Table columnsW columnsR
  -> (columnsR -> Column PGBool)
  -> Db Int64
liftDelete t w = withConnection (\ con -> runDelete con t w)

withConnection :: (Connection -> IO a) -> Db a
withConnection f = do
  c <- view dbEnvConnection
  wrapExceptions (f c)
    [ Handler (pure . DbSqlError)
    , Handler (pure . DbQueryError)
    ]
