module Poppy.Db
  ( Db (..),
    DbPool (..),
    DbEnv (..),
    PoolConfig (..),
    SqlLogger,
    defaultPool,
    connect,
    connectWith,
    closePool,
    runDb,
    withTransaction,
    transaction,
    transactionEither,
    withConn,
    dbIO,
    logSql,
    liftIO,
  )
where

import Control.Exception (handle, throwIO)
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, setNumStripes, withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG
import Poppy.Errors (ORMError)

-- | Assembled SQL only; bound parameters are not logged.
type SqlLogger = Text -> IO ()

data PoolConfig = PoolConfig
  { poolStripes :: Int,
    poolMaxPerStripe :: Int,
    poolIdleSeconds :: Double,
    poolSqlLog :: SqlLogger
  }

defaultPool :: PoolConfig
defaultPool =
  PoolConfig
    { poolStripes = 4,
      poolMaxPerStripe = 5,
      poolIdleSeconds = 10,
      poolSqlLog = \_ -> pure ()
    }

data DbPool = DbPool
  { unDbPool :: Pool Connection,
    dbSqlLog :: SqlLogger
  }

data DbEnv = DbEnv
  { dbConnection :: Connection,
    dbLogSql :: SqlLogger
  }

newtype Db a = Db {unDb :: DbEnv -> IO a}

instance Functor Db where
  fmap f (Db g) = Db (fmap f . g)

instance Applicative Db where
  pure x = Db (const (pure x))
  Db f <*> Db x = Db $ \env -> f env <*> x env

instance Monad Db where
  Db m >>= f = Db $ \env -> do
    a <- m env
    unDb (f a) env

instance MonadIO Db where
  liftIO io = Db (const io)

dbIO :: (Connection -> IO a) -> Db a
dbIO action = Db $ \env -> action (dbConnection env)

logSql :: Text -> Db ()
logSql sql = Db $ \env -> dbLogSql env sql

connect :: String -> IO DbPool
connect = connectWith defaultPool

connectWith :: PoolConfig -> String -> IO DbPool
connectWith config databaseUrl = do
  let totalMaxConnections = poolStripes config * poolMaxPerStripe config
      poolConfig =
        setNumStripes (Just (poolStripes config)) $
          defaultPoolConfig
            (PG.connectPostgreSQL $ B8.pack databaseUrl)
            PG.close
            (poolIdleSeconds config)
            totalMaxConnections
  pool <- newPool poolConfig
  pure DbPool {unDbPool = pool, dbSqlLog = poolSqlLog config}

closePool :: DbPool -> IO ()
closePool (DbPool pool _) = destroyAllResources pool

runDb :: DbPool -> Db a -> IO a
runDb pool (Db action) =
  withResource (unDbPool pool) $ \conn ->
    action (DbEnv {dbConnection = conn, dbLogSql = dbSqlLog pool})

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn (DbPool pool _) = withResource pool

withTransaction :: DbPool -> Db a -> IO a
withTransaction pool (Db action) =
  withResource (unDbPool pool) $ \conn ->
    let env = DbEnv {dbConnection = conn, dbLogSql = dbSqlLog pool}
     in PG.withTransaction conn (action env)

-- | Wrap an in-flight 'Db' action in @BEGIN@/@COMMIT@ (vs 'withTransaction', which takes a pool).
transaction :: Db a -> Db a
transaction (Db action) = Db $ \env ->
  PG.withTransaction (dbConnection env) (action env)

-- | Like 'transaction', but 'Left' also rolls back. @postgresql-simple@ only
-- undoes the transaction when the action throws; 'ORMError' is rethrown inside
-- the transaction and caught afterwards so callers still see 'Either'.
transactionEither :: Db (Either ORMError a) -> Db (Either ORMError a)
transactionEither (Db action) = Db $ \env ->
  handle (pure . Left) $
    PG.withTransaction (dbConnection env) $ do
      result <- action env
      case result of
        Left err -> throwIO err
        Right val -> pure (Right val)
