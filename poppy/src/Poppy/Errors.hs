-- | Errors returned as @Either ORMError@ from Client operations.
module Poppy.Errors
  ( ORMError (..),
    DatabaseErrorInfo (..),
    DriverErrorKind (..),
    requireFound,
    fromUniqueRows,
    uniqueOrFail,
    parseSingleton,
  )
where

import Control.Exception (Exception)
import Data.Text (Text)

-- | SQLSTATE, message, and detail from Postgres.
data DatabaseErrorInfo = DatabaseErrorInfo
  { sqlState :: Text,
    message :: Text,
    detail :: Text
  }
  deriving (Show, Eq)

-- | @postgresql-simple@ failures that are not constraint violations.
data DriverErrorKind
  = FormatMismatch
  | ClientQuery
  | ResultDecode
  deriving (Show, Eq)

-- | Client and driver failures. Constraint violations are classified; other @SqlError@s are 'DatabaseError'.
data ORMError
  = RecordNotFound Text
  | MultipleRecordsFound Text
  | UniqueViolation Text
  | ForeignKeyViolation Text
  | NotNullViolation Text
  | -- | A write or lookup required a @WHERE@ and none was given.
    EmptyWhere Text
  | UnsupportedIncludeModifier Text
  | -- | Other Postgres @SqlError@ (includes SQLSTATE).
    DatabaseError DatabaseErrorInfo
  | DriverError DriverErrorKind Text
  deriving (Show, Eq)

instance Exception ORMError

requireFound :: Maybe a -> ORMError -> Either ORMError a
requireFound Nothing err = Left err
requireFound (Just value) _ = Right value

fromUniqueRows :: [a] -> Either ORMError (Maybe a)
fromUniqueRows rows =
  case rows of
    [] -> Right Nothing
    [row] -> Right (Just row)
    _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")

uniqueOrFail :: Either ORMError (Maybe a) -> Either ORMError a
uniqueOrFail (Left err) = Left err
uniqueOrFail (Right found) =
  requireFound found (RecordNotFound "No record found matching query")

parseSingleton :: [a] -> ORMError -> ORMError -> Either ORMError a
parseSingleton rows notFoundErr multipleErr =
  case rows of
    [] -> Left notFoundErr
    [row] -> Right row
    _ -> Left multipleErr
