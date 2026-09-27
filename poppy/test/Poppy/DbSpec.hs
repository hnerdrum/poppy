module Poppy.DbSpec
  ( dbSpec,
  )
where

import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy (DbPool, PoolConfig (..), closePool, connectWith, defaultPool, runDb)
import qualified Poppy.Operations as Ops
import Poppy.Query (matching)
import Poppy.Sql (executeRaw, param, queryRaw)
import Poppy.Where (eq)
import Schema.Widget (WidgetTable, widgetName)
import Support.TestDb (TestEnv, testDatabaseUrl)
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

data CountRow = CountRow {cnt :: Int64}
  deriving (Eq, Show)

instance FromRow CountRow where
  fromRow = CountRow <$> field

withCustomPool :: PoolConfig -> (DbPool -> IO a) -> IO a
withCustomPool config action = do
  url <- testDatabaseUrl
  bracket (connectWith config url) closePool action

dbSpec :: SpecWith TestEnv
dbSpec =
  describe "Poppy.Db pool config" $ do
    it "connectWith a one-connection pool still runs a query" $ \_ ->
      withCustomPool defaultPool {poolStripes = 1, poolMaxPerStripe = 1} $ \pool -> do
        rows <-
          runDb pool $
            queryRaw @CountRow (Query "SELECT 1 AS cnt") []
        rows `shouldBe` [CountRow 1]

    it "sql log hook receives the assembled statement with parameters redacted" $ \_ -> do
      logs <- newIORef []
      let config =
            defaultPool
              { poolStripes = 1,
                poolMaxPerStripe = 1,
                poolSqlLog = \sql -> modifyIORef' logs (sql :)
              }
      withCustomPool config $ \pool -> do
        _ <-
          runDb pool $
            queryRaw @CountRow
              (Query "SELECT COUNT(*) AS cnt FROM test_widget WHERE name = ?")
              [param ("redacted-param" :: Text)]
        _ <-
          runDb pool $
            executeRaw
              (Query "UPDATE test_widget SET name = name WHERE name = ?")
              [param ("redacted-param" :: Text)]
        _ <- runDb pool (Ops.count @WidgetTable $ matching (eq widgetName "redacted-param"))
        pure ()
      recorded <- reverse <$> readIORef logs
      recorded
        `shouldSatisfy` any (T.isInfixOf "SELECT COUNT(*) AS cnt FROM test_widget WHERE name = ?")
      recorded
        `shouldSatisfy` any (T.isInfixOf "UPDATE test_widget SET name = name WHERE name = ?")
      recorded
        `shouldSatisfy` any (T.isInfixOf "SELECT COUNT(*) FROM \"test_widget\"")
      recorded `shouldSatisfy` all (not . T.isInfixOf "redacted-param")
