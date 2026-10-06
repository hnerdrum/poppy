{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Include edges for generated @Schema.Include.*@ modules.
--
-- 'skip' omits a relation. 'load' fetches its rows, and 'loadWith' nests
-- another include. Record-update 'where_', 'orderBy_', and 'take_' on
-- 'load' or 'loadWith' to filter that edge. 'take_' is per parent.
-- Import the result constructor
-- (@BookWith (..)@), or a skipped field is reported as a missing @HasField@
-- instance.
module Poppy.Include
  ( Skip (..),
    Load (..),
    load,
    loadWith,
    skip,
    skipped,
    Skipped,
    ModelTable,
    IncludeFor,
    ValidEdge,
    requireRelated,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Poppy.Query (OrderBy)
import Poppy.Where (Where)

data Skip = Skip
  deriving (Show, Eq)

-- | One loaded relation.
--
-- 'include_' is the nested include, or @()@ when this edge stops here.
-- 'where_' and 'orderBy_' use the child table. 'take_' keeps that many
-- child rows for each parent; 'Nothing' keeps every match. Empty
-- 'orderBy_' sorts by the child primary key.
data Load table include = Load
  { include_ :: include,
    where_ :: Maybe (Where table),
    orderBy_ :: [OrderBy table],
    take_ :: Maybe Int
  }
  deriving (Show, Eq)

-- | Load every related row, with no nested include.
load :: Load table ()
load =
  Load
    { include_ = (),
      where_ = Nothing,
      orderBy_ = [],
      take_ = Nothing
    }

-- | Load related rows and nest @include_@. Filters match 'load'.
loadWith :: include -> Load table include
loadWith include_ =
  Load
    { include_ = include_,
      where_ = Nothing,
      orderBy_ = [],
      take_ = Nothing
    }

skip :: Skip
skip = Skip

-- | Placeholder stored in a skipped field. Forcing it is a type error
-- ('Skipped'), so this value is only for constructing the result.
skipped :: a
skipped = error "Poppy: relation was not included"

type Skipped (name :: Symbol) (loaded :: Type) =
  TypeError
    ( Text "'"
        :<>: Text name
        :<>: Text "' was skipped: NotIncluded vs "
        :<>: ShowType loaded
    )

-- | Generated: @type instance ModelTable \"Book\" = BookTable@.
-- Ties an include edge to the child table so 'where_' cannot target
-- a different model.
type family ModelTable (model :: Symbol) :: Type

class IncludeFor (model :: Symbol) (include :: Type)

type family ValidEdge (model :: Symbol) (edge :: Type) :: Constraint where
  ValidEdge _ Skip = ()
  ValidEdge model (Load table ()) = table ~ ModelTable model
  ValidEdge model (Load table include) =
    (table ~ ModelTable model, IncludeFor model include)
  ValidEdge model other =
    TypeError
      ( Text "A "
          :<>: Text model
          :<>: Text " relation is skip or load, got "
          :<>: ShowType other
      )

-- | A required belongs-to whose parent row is missing.
requireRelated :: String -> Maybe a -> a
requireRelated name Nothing =
  error ("Poppy: required relation '" ++ name ++ "' row was missing")
requireRelated _ (Just row) = row
