module Db
  ( DBConnectionString
  , initConnectionPool
  , migrate
  )
where

import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           RIO

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr) close 2 60 10

migrate :: FilePath -> Connection -> IO (MigrationResult String)
migrate dir conn = do
  _ <- withTransaction  conn $ runMigration $ MigrationContext MigrationInitialization True conn
  withTransaction conn $ runMigration $ MigrationContext
    (MigrationDirectory dir)
    True
    conn
