{-# LANGUAGE TypeApplications #-}

module Support.TestDb
  ( TestEnv (..),
    withTestDb,
    testDatabaseUrl,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, displayException, try)
import ORM.Db (DbPool, closePool, connect)
import Support.TestMigrations (runTestMigrations)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)

newtype TestEnv = TestEnv
  { envPool :: DbPool
  }

withTestDb :: SpecWith TestEnv -> Spec
withTestDb spec =
  beforeAll setupTestEnv $
    afterAll destroyTestEnv spec

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
