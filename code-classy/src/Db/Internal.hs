{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ConstraintKinds            #-}
module Db.Internal where

import Control.Error                   (headMay)
import Control.Lens                    (makeClassy, makeClassyPrisms, view,
                                        Prism',(#))
import Control.Exception               (SomeException,catches)
import Control.Exception.Lens          (exception)
import Control.Monad.Except            (MonadError,throwError)
import Control.Monad.Error.Lens        (handler)
import Control.Monad.Reader            (MonadReader)
import Control.Monad.Trans             (MonadIO,liftIO)
import Data.Int                        (Int64)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (Connection, QueryError, SqlError,
                                        close)
import Opaleye                         (Column, PGBool, Query, QueryRunner,
                                        Table, Unpackspec, runDelete,
                                        runInsert, runInsertReturning,
                                        runQuery, runUpdate)

import Utils (wrapExceptions)

data DbError
  = DbQueryError QueryError
  | DbSqlError SqlError
  deriving (Show)
makeClassyPrisms ''DbError

data DbEnv = DbEnv
  { _dbEnvConnection :: Connection
  }
makeClassy ''DbEnv

type CanDb c e m =
  ( ProvidesDbEnv c m
  , CanDbError e m
  , MonadIO m
  )

type ProvidesDbEnv c m = (MonadReader c m, HasDbEnv c)
type CanDbError e m    = (MonadError e m, AsDbError e)

liftQuery
  :: ( CanDb c e m, Default QueryRunner columnsW haskells )
  => Query columnsW
  -> m [haskells]
liftQuery q = withConnection (`runQuery` q)

liftQueryFirst
  :: ( CanDb c e m, Default QueryRunner columnsW haskells )
  => Query columnsW
  -> m (Maybe haskells)
liftQueryFirst = fmap headMay . liftQuery

liftInsert
  :: CanDb c e m
  => Table columnsW columnsR
  -> columnsW
  -> m Int64
liftInsert t c = withConnection (\ con -> runInsert con t c)

liftInsertReturning
  :: ( CanDb c e m
     , Default QueryRunner returned haskells
     , Default Unpackspec returned returned
     )
  => Table columnsW columnsR
  -> (columnsR -> returned)
  -> columnsW
  -> m [haskells]
liftInsertReturning t f c = withConnection (\ con -> runInsertReturning con t c f)

liftInsertReturningFirst
  :: ( CanDb c e m
     , Default QueryRunner returned haskells
     , Default Unpackspec returned returned
     )
  => Table columnsW columnsR
  -> (columnsR -> returned)
  -> columnsW
  -> m haskells
liftInsertReturningFirst t f = fmap head . liftInsertReturning t f

liftUpdate
  :: CanDb c e m
  => Table columnsW columnsR
  -> (columnsR -> columnsW)
  -> (columnsR -> Column PGBool)
  -> m Int64
liftUpdate t f w = withConnection (\ con -> runUpdate con t f w)

liftDelete
  :: CanDb c e m
  => Table columnsW columnsR
  -> (columnsR -> Column PGBool)
  -> m Int64
liftDelete t w = withConnection (\ con -> runDelete con t w)

_SqlError :: Prism' SomeException SqlError
_SqlError = exception

_QueryError :: Prism' SomeException QueryError
_QueryError = exception

withConnection :: CanDb c e m => (Connection -> IO a) -> m a
withConnection f = do
  c <- view dbEnvConnection
  wrapExceptions (f c)
    [ handler _SqlError   (pure . (_DbSqlError #))
    , handler _QueryError (pure . (_DbQueryError #))
    ]
