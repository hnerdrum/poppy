{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Support.TestDb
  ( TestEnv (..),
    withTestDb,
    testDatabaseUrl,
    resetTestData,
    truncateTable,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types (Query (..))
import ORM.Core (Entity (..))
import ORM.Db (DbPool, closePool, connect, withConn)
import ORM.Sql (quoteIdent)
import Schema.Book (BookTable)
import Schema.Chapter (ChapterTable)
import Schema.Section (SectionTable)
import Schema.Shelf (ShelfTable)
import Schema.Tag (TagTable)
import Schema.Widget (WidgetTable)
import Support.TestMigrations (runTestMigrations)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll, beforeWith)

newtype TestEnv = TestEnv
  { envPool :: DbPool
  }

withTestDb :: SpecWith TestEnv -> Spec
withTestDb spec =
  beforeAll setupTestEnv $
    afterAll destroyTestEnv $
      beforeWith (\env -> resetTestData env >> return env) spec

testDatabaseUrl :: IO String
testDatabaseUrl = do
  mTestUrl <- lookupEnv "TEST_DATABASE_URL"
  mDbUrl <- lookupEnv "DATABASE_URL"
  case mTestUrl <|> mDbUrl of
    Nothing ->
      fail $
        unlines
          [ "No test database URL configured.",
            "Set TEST_DATABASE_URL (recommended) or DATABASE_URL.",
            "Start Postgres with: docker compose up -d",
            "Default URL: postgres://poppy:poppy@127.0.0.1:5435/poppy_test"
          ]
    Just url -> return url

setupTestEnv :: IO TestEnv
setupTestEnv = do
  databaseUrl <- testDatabaseUrl
  result <- try @SomeException $ do
    runTestMigrations databaseUrl
    connect databaseUrl
  case result of
    Left err -> fail (connectionErrorMessage databaseUrl err)
    Right pool -> return TestEnv {envPool = pool}

connectionErrorMessage :: String -> SomeException -> String
connectionErrorMessage databaseUrl err =
  unlines
    [ "Could not connect to the test database.",
      displayException err,
      "",
      "URL: " <> databaseUrl,
      "",
      "Start the test database with:",
      "  docker compose up -d",
      "",
      "Then:",
      "  TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test cabal test all"
    ]

destroyTestEnv :: TestEnv -> IO ()
destroyTestEnv TestEnv {envPool = pool} =
  closePool pool

resetTestData :: TestEnv -> IO ()
resetTestData env = do
  truncateTable @WidgetTable env
  truncateTable @SectionTable env
  truncateTable @ChapterTable env
  truncateTable @BookTable env
  truncateTable @TagTable env
  truncateTable @ShelfTable env

truncateTable ::
  forall table.
  (Entity table) =>
  TestEnv ->
  IO ()
truncateTable TestEnv {envPool = pool} =
  withConn pool $ \conn ->
    void $
      PG.execute_
        conn
        (Query (TE.encodeUtf8 ("TRUNCATE " <> quoteIdent (tableName @table) <> " RESTART IDENTITY CASCADE" :: Text)))
