module Support.TestMigrations
  ( runTestMigrations,
  )
where

import Control.Exception (Handler (..), catches, throwIO)
import Control.Monad (forM_, void)
import qualified Data.ByteString.Char8 as B8
import Data.Char (isSpace)
import Data.List (sort)
import Database.PostgreSQL.Simple (Connection, SqlError (..), connectPostgreSQL, execute_)
import Database.PostgreSQL.Simple.Types (Query (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))

testMigrationsDir :: FilePath
testMigrationsDir = "test/migrations"

runTestMigrations :: String -> IO ()
runTestMigrations databaseUrl = do
  conn <- connectPostgreSQL $ B8.pack databaseUrl
  names <- sort <$> listDirectory testMigrationsDir
  forM_ names $ \name -> do
    sql <- B8.readFile (testMigrationsDir </> name)
    forM_ (splitStatements sql) $ \stmt ->
      executeIgnoringDuplicate conn stmt

splitStatements :: B8.ByteString -> [B8.ByteString]
splitStatements =
  filter (not . B8.null) . map stripBytes . B8.split ';'

executeIgnoringDuplicate :: Connection -> B8.ByteString -> IO ()
executeIgnoringDuplicate conn stmt =
  void (execute_ conn (Query stmt))
    `catches` [Handler ignoreDuplicate]
  where
    ignoreDuplicate err
      | sqlState err == "42710" = pure ()
      | otherwise = throwIO err

stripBytes :: B8.ByteString -> B8.ByteString
stripBytes =
  B8.reverse . B8.dropWhile isSpace . B8.reverse . B8.dropWhile isSpace
