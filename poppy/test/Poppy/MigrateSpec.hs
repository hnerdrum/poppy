module Poppy.MigrateSpec
  ( migrateSpec,
  )
where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Only (..))
import qualified Database.PostgreSQL.Simple as PG
import Poppy (MigrateError (..), applyMigrations)
import Poppy.Db (DbPool, withConn)
import Support.TestDb (TestEnv (..))
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

migrateSpec :: SpecWith TestEnv
migrateSpec =
  describe "Poppy.Migrate" $ do
    it "applies SQL files in name order and is a no-op on replay" $ \TestEnv {envPool = pool} ->
      withCleanProbe pool
        $ withMigrationDir
          [ ("notes.txt", "not sql"),
            ("002-add-label.sql", "ALTER TABLE _poppy_phase10_probe ADD COLUMN label TEXT;"),
            ("001-create.sql", "CREATE TABLE _poppy_phase10_probe (id INT PRIMARY KEY);")
          ]
        $ \dir -> do
          first <- applyMigrations pool dir
          first `shouldBe` Right ["001-create.sql", "002-add-label.sql"]
          columns <- probeColumns pool
          columns `shouldBe` 2
          second <- applyMigrations pool dir
          second `shouldBe` Right []
          probeColumns pool >>= (`shouldBe` 2)

    it "does not record a failed file" $ \TestEnv {envPool = pool} ->
      withCleanProbe pool
        $ withMigrationDir
          [ ("001-ok.sql", "CREATE TABLE _poppy_phase10_probe (id INT PRIMARY KEY);"),
            ("002-bad.sql", "SELECT * FROM definitely_not_a_poppy_migrate_table;")
          ]
        $ \dir -> do
          result <- applyMigrations pool dir
          result
            `shouldSatisfy` ( \case
                                Left (MigrateFailed "002-bad.sql" _) -> True
                                _ -> False
                            )
          recorded <- recordedNames pool
          recorded `shouldBe` ["001-ok.sql"]
          exists <- probeExists pool
          exists `shouldBe` True

    it "returns DirectoryError when the path is not a directory" $ \TestEnv {envPool = pool} -> do
      result <- applyMigrations pool "/definitely-not-a-poppy-migrate-dir"
      result
        `shouldSatisfy` ( \case
                            Left (MigrateDirectoryError _) -> True
                            _ -> False
                        )

withCleanProbe :: DbPool -> IO a -> IO a
withCleanProbe pool action = dropProbeState pool >> action <* dropProbeState pool

dropProbeState :: DbPool -> IO ()
dropProbeState pool =
  withConn pool $ \conn -> do
    _ <- PG.execute_ conn "DROP TABLE IF EXISTS _poppy_phase10_probe CASCADE"
    _ <- PG.execute_ conn "DROP TABLE IF EXISTS _poppy_migrations CASCADE"
    pure ()

withMigrationDir :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withMigrationDir files action = do
  tmp <- getTemporaryDirectory
  token <- nextRandom
  let dir = tmp </> ("poppy-migrate-" <> show token)
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    ( \dir -> do
        mapM_ (\(name, body) -> TIO.writeFile (dir </> name) body) files
        action dir
    )

probeExists :: DbPool -> IO Bool
probeExists pool =
  withConn pool $ \conn -> do
    [Only exists] <-
      PG.query_
        conn
        "SELECT to_regclass('public._poppy_phase10_probe') IS NOT NULL"
    pure exists

probeColumns :: DbPool -> IO Int64
probeColumns pool =
  withConn pool $ \conn -> do
    [Only n] <-
      PG.query_
        conn
        "SELECT COUNT(*) FROM information_schema.columns \
        \WHERE table_schema = 'public' AND table_name = '_poppy_phase10_probe'"
    pure n

recordedNames :: DbPool -> IO [Text]
recordedNames pool =
  withConn pool $ \conn -> do
    rows <- PG.query_ conn "SELECT name FROM _poppy_migrations ORDER BY name"
    pure [name | Only name <- rows]
