module Poppy.Db
  ( Db (..),
    DbPool (..),
    connect,
    closePool,
    runDb,
    withTransaction,
    transaction,
    withConn,
    liftIO,
  )
where

import Control.Monad.IO.Class (MonadIO (..), liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, setNumStripes, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG

newtype DbPool = DbPool {unDbPool :: Pool Connection}

newtype Db a = Db {unDb :: Connection -> IO a}

instance Functor Db where
  fmap f (Db g) = Db (fmap f . g)

instance Applicative Db where
  pure x = Db (const (pure x))
  Db f <*> Db x = Db $ \conn -> f conn <*> x conn

instance Monad Db where
  Db m >>= f = Db $ \conn -> do
    a <- m conn
    unDb (f a) conn

instance MonadIO Db where
  liftIO io = Db (const io)

connect :: String -> IO DbPool
connect databaseUrl = do
  let numStripes = 4
      keepOpenTime = 10
      maxResourcesPerStripe = 5
      totalMaxConnections = numStripes * maxResourcesPerStripe
      poolConfig =
        setNumStripes (Just numStripes) $
          defaultPoolConfig
            (PG.connectPostgreSQL $ B8.pack databaseUrl)
            PG.close
            keepOpenTime
            totalMaxConnections
  DbPool <$> newPool poolConfig

closePool :: DbPool -> IO ()
closePool (DbPool pool) = destroyAllResources pool

runDb :: DbPool -> Db a -> IO a
runDb (DbPool pool) (Db action) = withResource pool action

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn (DbPool pool) = withResource pool

withTransaction :: DbPool -> Db a -> IO a
withTransaction (DbPool pool) (Db action) =
  withResource pool $ \conn -> PG.withTransaction conn (action conn)

transaction :: Db a -> Db a
transaction (Db action) = Db $ \conn -> PG.withTransaction conn (action conn)
