{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.Core
  ( Entity (..),
    Column (..),
    SqlType (..),
    Field (..),
    NullableValue (..),
    PrimaryKeyType,
  )
where

import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

class SqlType a where
  toSqlType :: Text

class (FromField a, ToField a) => Column a where
  columnType :: Text

data Field table a = Field
  { fieldName :: Text,
    fieldColumn :: Text
  }

class Entity table where
  tableName :: Text
  primaryKey :: Field table (PrimaryKeyType table)
  tableColumns :: [Text]
  uniqueKeys :: [[Text]]
  uniqueKeys = [[fieldColumn (primaryKey @table)]]

type family PrimaryKeyType table

-- | Three-way column input for nullable fields on create and update.
-- 'Omit' leaves the column out; 'Value' sets it; 'Null' sets SQL NULL.
data NullableValue a
  = Omit
  | Value a
  | Null
  deriving (Show, Eq)
