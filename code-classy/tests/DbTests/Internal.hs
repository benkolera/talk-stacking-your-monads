{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds   #-}
module DbTests.Internal where

import BasePrelude

import           Control.Lens                     ((^.))
import           Control.Monad.Random
import           Control.Monad.Except             (ExceptT,runExceptT)
import           Control.Monad.Reader             (ReaderT,runReaderT)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           Test.Tasty.HUnit

import Db

rnd :: (RandomGen g) => Rand g Char
rnd = getRandomR ('a','z')

dbName :: IO String
dbName = ("transactions_test_" <>) <$> evalRandIO (sequence (replicate 10 rnd))

assertDbResult :: Either DbError a -> (a -> Assertion) -> Assertion
assertDbResult e f = either (error . ("DB Failed: " <>) . show) f e

roundTripTest
  :: (Eq a, Show a)
  => String
  -> (n -> ExceptT DbError (ReaderT DbEnv IO) i)
  -> (i -> ExceptT DbError (ReaderT DbEnv IO) (Maybe a))
  -> (n -> i -> a)
  -> n
  -> Assertion
roundTripTest testName insertN queryA newToExisting new =
  withDb testName $ \ e -> do
    res <- runDb e $ do
      i <- insertN new
      a <- queryA i
      pure (i,a)
    assertDbResult res $ \ (i,a) -> Just (newToExisting new i) @=? a

runDb :: DbEnv -> ExceptT DbError (ReaderT DbEnv IO) a -> IO (Either DbError a)
runDb env = flip runReaderT env . runExceptT 

withDb :: String -> (DbEnv -> Assertion) -> Assertion
withDb testName f = do
  pc <- connectPostgreSQL "dbname=postgres"
  n  <- dbName
  let nb = B8.pack n
  bracket
    (setup pc nb)
    (cleanup pc nb)
    f

  where
    setup pc nb = do
      void . execute_ pc . Query $ "CREATE DATABASE " <> nb
      tc <- connectPostgreSQL ("dbname=" <> nb)
      schemaSql <- B.readFile "schema.sql"
      void . execute_ tc . Query $ schemaSql
      dataSql   <- B.readFile $ "tests/sql/" <> testName <> ".sql"
      void . execute_ tc . Query $ dataSql
      pure (DbEnv tc)

    cleanup pc nb tc = do
      close $ tc^.dbEnvConnection
      void . execute_ pc . Query $ "DROP DATABASE " <> nb
