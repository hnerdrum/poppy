{-# OPTIONS_GHC -Wno-orphans #-}

module Poppy.Column
  ( Column (..),
  )
where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField ()
import Database.PostgreSQL.Simple.ToField ()
import Poppy.Core (Column (..))

instance Column UUID where
  columnType = "uuid"

instance Column Text where
  columnType = "text"

instance Column Int where
  columnType = "integer"

instance Column Int32 where
  columnType = "integer"

instance Column Int64 where
  columnType = "bigint"

instance Column UTCTime where
  columnType = "timestamp with time zone"

instance Column Bool where
  columnType = "boolean"
