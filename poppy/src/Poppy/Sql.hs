module Poppy.Sql
  ( Param (..),
    param,
    queryRaw,
    executeRaw,
    catchDb,
    catchSql,
    fromSqlError,
    quoteIdent,
    quoteQualified,
  )
where

import Control.Exception (Handler (..), catches)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Database.PostgreSQL.Simple
  ( FormatError (..),
    QueryError (..),
    SqlError (..),
  )
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromField (ResultError (..))
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Db (Db (..))
import Poppy.Errors (DatabaseErrorInfo (..), DriverErrorKind (..), ORMError (..))

newtype Param = Param {unParam :: Action}

param :: (ToField a) => a -> Param
param = Param . toField

quoteIdent :: Text -> Text
quoteIdent name = "\"" <> T.replace "\"" "\"\"" name <> "\""

quoteQualified :: Text -> Text -> Text
quoteQualified alias col = quoteIdent alias <> "." <> quoteIdent col

queryRaw :: (FromRow r) => Query -> [Param] -> Db [r]
queryRaw query params =
  Db $ \conn ->
    PGSimple.queryWith fromRow conn query (map unParam params)

executeRaw :: Query -> [Param] -> Db Int
executeRaw query params =
  Db $ \conn ->
    fromIntegral <$> PGSimple.execute conn query (map unParam params)

catchDb :: Db a -> Db (Either ORMError a)
catchDb (Db action) = Db $ \conn -> catchSql (action conn)

catchSql :: IO a -> IO (Either ORMError a)
catchSql action =
  (Right <$> action)
    `catches` [ Handler (pure . Left . fromSqlError),
                Handler (pure . Left . fromFormatError),
                Handler (pure . Left . fromQueryError),
                Handler (pure . Left . fromResultError)
              ]

fromSqlError :: SqlError -> ORMError
fromSqlError SqlError {sqlState = state, sqlErrorMsg = msg, sqlErrorDetail = detail}
  | state == "23505" = UniqueViolation (decode msg)
  | state == "23503" = ForeignKeyViolation (decode msg)
  | state == "23502" = NotNullViolation (decode msg)
  | otherwise =
      databaseError (decode state) (decode msg) (decode detail)
  where
    decode = decodeUtf8With lenientDecode

fromFormatError :: FormatError -> ORMError
fromFormatError FormatError {fmtMessage} =
  DriverError FormatMismatch (T.pack fmtMessage)

fromQueryError :: QueryError -> ORMError
fromQueryError QueryError {qeMessage} =
  DriverError ClientQuery (T.pack qeMessage)

fromResultError :: ResultError -> ORMError
fromResultError err =
  DriverError ResultDecode (T.pack (resultMessage err))
  where
    resultMessage Incompatible {errMessage} = errMessage
    resultMessage UnexpectedNull {errMessage} = errMessage
    resultMessage ConversionFailed {errMessage} = errMessage

databaseError :: Text -> Text -> Text -> ORMError
databaseError sqlState message detail =
  DatabaseError DatabaseErrorInfo {sqlState, message, detail}
