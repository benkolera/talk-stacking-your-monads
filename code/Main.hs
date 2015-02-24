{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import BasePrelude                hiding (left)
import Data.Configurator
import Data.Configurator.Types    (Config)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, connect,
                                   defaultConnectInfo)

import App
import Control.Error
import Csv           (CsvError (..))
import Db            (DbEnv (..), DbError (..))

main :: IO ()
main = runScript $ do
  args <- scriptIO $ getArgs
  env  <- loadConfig
  fn   <- headMay args ?? usage
  e <- scriptIO . runApp env . loadAndInsert $ fn
  scriptIO . putStrLn . either appErrorString successString $ e

loadConfig :: Script AppEnv
loadConfig = fmapLT ("There were problems loading the config:\n" <>) $ do
  conf <- scriptIO $ load [Required "app.cfg"]
  conn <- scriptIO $ requireConnection conf
  pure $ AppEnv (DbEnv conn)

requireConnection :: Config -> IO Connection
requireConnection c = do
  host <- lookupDefault (connectHost defaultConnectInfo) c "db.host"
  port <- lookupDefault (connectPort defaultConnectInfo) c "db.port"
  user <- lookupDefault (connectUser defaultConnectInfo) c "db.user"
  pass <- lookupDefault (connectPassword defaultConnectInfo) c "db.password"
  db   <- lookupDefault (connectDatabase defaultConnectInfo) c "db.database"
  connect (ConnectInfo host port user pass db)

-- This can also be achieved with the prisms from Control.Lens
appErrorString :: AppError -> String
appErrorString (AppDbError db)   = dbErrorString db
appErrorString (AppCsvError csv) = csvErrorString csv

dbErrorString :: DbError -> String
dbErrorString (DbQueryError q) = "There was a problem with one of the sql queries: " <> show q
dbErrorString (DbSqlError s) = "There was a problem connecting to the database: " <> show s

csvErrorString :: CsvError -> String
csvErrorString (CsvIoError i)          = "There was file error reading the csv file: " <> show i
csvErrorString (CsvHeaderParseError s) = "Failed parsing the csv header to get the account number: " <> s
csvErrorString (CsvDecodeErrors s)     = unlines $ "Some csv lines failed to parse:" : s

successString :: [Int] -> String
successString ids = "Great Success! " <> (show . length $ ids) <> " rows inserted"

usage :: String
usage = "missing file name: run with cabal run -- <filename>"
