module Poppy.Errors
  ( ORMError (..),
    DatabaseErrorInfo (..),
    DriverErrorKind (..),
    requireFound,
    parseSingleton,
  )
where

import Control.Exception (Exception)
import Data.Text (Text)

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

data ORMError
  = RecordNotFound Text
  | MultipleRecordsFound Text
  | UniqueViolation Text
  | ForeignKeyViolation Text
  | NotNullViolation Text
  | -- | A write or lookup required a @WHERE@ and none was given.
    EmptyWhere Text
  | -- | @findUnique@ @where_@ was not a primary key or declared unique.
    InvalidUniqueInput Text
  | UnsupportedIncludeModifier Text
  | DatabaseError DatabaseErrorInfo
  | DriverError DriverErrorKind Text
  deriving (Show, Eq)

instance Exception ORMError

requireFound :: Maybe a -> ORMError -> Either ORMError a
requireFound Nothing err = Left err
requireFound (Just value) _ = Right value

parseSingleton :: [a] -> ORMError -> ORMError -> Either ORMError a
parseSingleton rows notFoundErr multipleErr =
  case rows of
    [] -> Left notFoundErr
    [row] -> Right row
    _ -> Left multipleErr
