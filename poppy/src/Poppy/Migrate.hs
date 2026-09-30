{-# LANGUAGE TypeApplications #-}

module Poppy.Migrate
  ( MigrateError (..),
    applyMigrations,
  )
where

import Control.Exception (IOException, displayException, try)
import Control.Monad (filterM, void)
import qualified Data.ByteString as B8
import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..))
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Db (DbPool, withConn)
import Poppy.Errors (ORMError)
import Poppy.Sql (catchSql)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

data MigrateError
  = MigrateDirectoryError Text
  | MigrateFailed Text ORMError
  deriving (Show, Eq)

-- | Apply @*.sql@ files in name order. Names already in @_poppy_migrations@
-- are skipped. A failed file is not recorded (same transaction as the SQL).
applyMigrations :: DbPool -> FilePath -> IO (Either MigrateError [Text])
applyMigrations pool dir = do
  listed <- listMigrationFiles dir
  case listed of
    Left err -> pure (Left err)
    Right files -> withConn pool $ \conn -> do
      ensureHistoryTable conn
      applied <- fetchApplied conn
      let pending = filter (\(name, _) -> name `notElem` applied) files
      applyPending conn pending

listMigrationFiles :: FilePath -> IO (Either MigrateError [(Text, FilePath)])
listMigrationFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Left (MigrateDirectoryError ("not a directory: " <> T.pack dir)))
    else do
      names <- listDirectory dir
      let sqlNames = sort (filter isMigrationName names)
      fileNames <- filterM (\name -> doesFileExist (dir </> name)) sqlNames
      pure (Right [(T.pack name, dir </> name) | name <- fileNames])

isMigrationName :: FilePath -> Bool
isMigrationName name =
  takeExtension name == ".sql" && not ("." `isPrefixOf` name)

ensureHistoryTable :: Connection -> IO ()
ensureHistoryTable conn =
  void (PG.execute_ conn historyTableSql)

historyTableSql :: Query
historyTableSql =
  "CREATE TABLE IF NOT EXISTS _poppy_migrations (\
  \ name TEXT PRIMARY KEY,\
  \ applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP\
  \)"

fetchApplied :: Connection -> IO [Text]
fetchApplied conn = do
  rows <- PG.query_ conn "SELECT name FROM _poppy_migrations"
  pure [name | Only name <- rows]

applyPending ::
  Connection ->
  [(Text, FilePath)] ->
  IO (Either MigrateError [Text])
applyPending conn = go []
  where
    go acc [] = pure (Right (reverse acc))
    go acc ((name, path) : rest) = do
      result <- applyOne conn name path
      case result of
        Left err -> pure (Left err)
        Right () -> go (name : acc) rest

applyOne :: Connection -> Text -> FilePath -> IO (Either MigrateError ())
applyOne conn name path = do
  sqlResult <- try @IOException (B8.readFile path)
  case sqlResult of
    Left ex ->
      pure (Left (MigrateDirectoryError (T.pack (displayException ex))))
    Right sql -> do
      result <-
        catchSql $
          PG.withTransaction conn $ do
            _ <- PG.execute_ conn (Query sql)
            _ <- PG.execute conn insertHistorySql (Only name)
            pure ()
      case result of
        Left err -> pure (Left (MigrateFailed name err))
        Right () -> pure (Right ())

insertHistorySql :: Query
insertHistorySql = "INSERT INTO _poppy_migrations (name) VALUES (?)"
