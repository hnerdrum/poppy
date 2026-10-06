{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Include edges for generated @Schema.Include.*@ modules.
--
-- 'skip' omits a relation, 'load' fetches its rows, and 'loadWith' nests
-- another include. 'Skipped' is a type error. Import the result constructor
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
    IncludeFor,
    ValidEdge,
    requireRelated,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

data Skip = Skip
  deriving (Show, Eq)

newtype Load include = Load include
  deriving (Show, Eq)

load :: Load ()
load = Load ()

loadWith :: include -> Load include
loadWith = Load

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

class IncludeFor (model :: Symbol) (include :: Type)

type family ValidEdge (model :: Symbol) (edge :: Type) :: Constraint where
  ValidEdge _ Skip = ()
  ValidEdge _ (Load ()) = ()
  ValidEdge model (Load include) = IncludeFor model include
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
