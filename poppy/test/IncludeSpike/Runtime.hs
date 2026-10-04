{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IncludeSpike.Runtime
  ( Skip (..),
    Load (..),
    NotIncluded (..),
    load,
    loadWith,
    skip,
    skipped,
    Skipped,
    IncludeFor,
    ValidEdge,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

data Skip = Skip
  deriving (Show, Eq)

newtype Load include = Load include
  deriving (Show, Eq)

data NotIncluded = NotIncluded
  deriving (Show, Eq)

load :: Load ()
load = Load ()

loadWith :: include -> Load include
loadWith = Load

skip :: Skip
skip = Skip

-- | Placeholder for a skipped field. The field's type is 'Skipped', so a
-- caller cannot force this value.
skipped :: a
skipped = error "IncludeSpike: relation was not included"

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
