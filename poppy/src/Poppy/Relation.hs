{-# LANGUAGE AllowAmbiguousTypes #-}

module Poppy.Relation
  ( JoinType (..),
    HasMany (..),
    BelongsTo (..),
  )
where

import Poppy.Core (Field)

data JoinType = LeftJoin | InnerJoin | RightJoin
  deriving (Show, Eq)

data HasMany parent child key = HasMany
  { localKey :: Field parent key,
    foreignKey :: Field child key,
    joinType :: JoinType
  }

data BelongsTo child parent key = BelongsTo
  { foreignKey :: Field child key,
    references :: Field parent key,
    joinType :: JoinType
  }
