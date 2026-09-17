{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Introspect
  ( introspectCatalog,
    canonicalizeColumnType,
  )
where

import Data.Int (Int32)
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query_)
import Database.PostgreSQL.Simple.Types (Query)
import Poppy.Codegen.Drift (DbCatalog (..), DbColumn (..), DbTable (..), emptyCatalog)

type ColumnRow = (Text, Text, Text, Text, Text)

type PkRow = (Text, Text, Int32)

type UniqueRow = (Text, Text, Text, Int32)

type EnumRow = (Text, Text)

introspectCatalog :: Connection -> IO DbCatalog
introspectCatalog conn = do
  columns <- query_ conn columnSql
  pks <- query_ conn pkSql
  uniques <- query_ conn uniqueSql
  enums <- query_ conn enumSql
  pure $
    emptyCatalog
      { dbTables = buildTables columns pks uniques,
        dbEnums = buildEnums enums
      }

canonicalizeColumnType :: Text -> Text -> Text
canonicalizeColumnType dataType udtName =
  case dataType of
    "text" -> "text"
    "uuid" -> "uuid"
    "integer" -> "int"
    "timestamp with time zone" -> "timestamptz"
    "boolean" -> "boolean"
    "USER-DEFINED" -> "enum:" <> udtName
    _ -> udtName

buildTables :: [ColumnRow] -> [PkRow] -> [UniqueRow] -> Map Text DbTable
buildTables columns pks uniques =
  Map.mapWithKey
    ( \name table ->
        table
          { dbPrimaryKey = Map.findWithDefault [] name pkMap,
            dbUniques = Map.findWithDefault [] name uniqueMap
          }
    )
    columnTables
  where
    columnTables = foldl' addColumn Map.empty columns
    pkMap = Map.fromListWith (flip (++)) [(table, [col]) | (table, col, _) <- sortOn pkOrd pks]
    uniqueMap = uniqueSets uniques

addColumn :: Map Text DbTable -> ColumnRow -> Map Text DbTable
addColumn acc (table, col, nullable, dataType, udtName) =
  Map.alter upsert table acc
  where
    column =
      DbColumn
        { dbColName = col,
          dbColType = canonicalizeColumnType dataType udtName,
          dbColNullable = nullable == "YES"
        }
    upsert Nothing =
      Just
        DbTable
          { dbTableName = table,
            dbColumns = Map.singleton col column,
            dbPrimaryKey = [],
            dbUniques = []
          }
    upsert (Just existing) =
      Just existing {dbColumns = Map.insert col column (dbColumns existing)}

pkOrd :: PkRow -> (Text, Int32)
pkOrd (table, _, pos) = (table, pos)

uniqueSets :: [UniqueRow] -> Map Text [Set Text]
uniqueSets rows =
  Map.fromListWith
    (++)
    [ (table, [Set.fromList (map (\(_, _, col, _) -> col) grouped)])
      | grouped@((table, _, _, _) : _) <- groupByConstraint (sortOn uniqueOrd rows)
    ]

uniqueOrd :: UniqueRow -> (Text, Text, Int32)
uniqueOrd (table, constraint, _, pos) = (table, constraint, pos)

groupByConstraint :: [UniqueRow] -> [[UniqueRow]]
groupByConstraint [] = []
groupByConstraint (row : rest) =
  let (same, others) = span (sameConstraint row) rest
   in (row : same) : groupByConstraint others

sameConstraint :: UniqueRow -> UniqueRow -> Bool
sameConstraint (tableA, nameA, _, _) (tableB, nameB, _, _) =
  tableA == tableB && nameA == nameB

buildEnums :: [EnumRow] -> Map Text [Text]
buildEnums rows =
  Map.fromListWith (flip (++)) [(name, [label]) | (name, label) <- rows]

columnSql :: Query
columnSql =
  "SELECT table_name, column_name, is_nullable, data_type, udt_name\
  \ FROM information_schema.columns\
  \ WHERE table_schema = 'public'"

pkSql :: Query
pkSql =
  "SELECT kcu.table_name, kcu.column_name, kcu.ordinal_position\
  \ FROM information_schema.table_constraints tc\
  \ JOIN information_schema.key_column_usage kcu\
  \   ON tc.constraint_name = kcu.constraint_name\
  \  AND tc.table_schema = kcu.table_schema\
  \ WHERE tc.table_schema = 'public'\
  \   AND tc.constraint_type = 'PRIMARY KEY'\
  \ ORDER BY kcu.table_name, kcu.ordinal_position"

uniqueSql :: Query
uniqueSql =
  "SELECT kcu.table_name, tc.constraint_name, kcu.column_name, kcu.ordinal_position\
  \ FROM information_schema.table_constraints tc\
  \ JOIN information_schema.key_column_usage kcu\
  \   ON tc.constraint_name = kcu.constraint_name\
  \  AND tc.table_schema = kcu.table_schema\
  \ WHERE tc.table_schema = 'public'\
  \   AND tc.constraint_type = 'UNIQUE'\
  \ ORDER BY kcu.table_name, tc.constraint_name, kcu.ordinal_position"

enumSql :: Query
enumSql =
  "SELECT t.typname, e.enumlabel\
  \ FROM pg_type t\
  \ JOIN pg_enum e ON t.oid = e.enumtypid\
  \ ORDER BY t.typname, e.enumsortorder"
