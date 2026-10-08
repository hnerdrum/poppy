{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-- | 'Column' instances for Schema scalars (including @numeric@ / 'Data.Scientific.Scientific' and @jsonb@ / 'Data.Aeson.Value').
module Poppy.Internal.Column
  ( Column (..),
  )
where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField ()
import Database.PostgreSQL.Simple.ToField ()
import Poppy.Internal.Core (Column (..))

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

instance Column Scientific where
  columnType = "numeric"

instance Column Value where
  columnType = "jsonb"

instance Column UTCTime where
  columnType = "timestamp with time zone"

instance Column Bool where
  columnType = "boolean"
