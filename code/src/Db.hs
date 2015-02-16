module Db where

import Database.PostgreSQL.Simple (Connection)

data DbEnv = DbEnv { _dbConnection :: Connection }
