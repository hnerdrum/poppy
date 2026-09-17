module ORM.Errors
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
  | EmptyWhere Text
  | InvalidUniqueInput Text
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
