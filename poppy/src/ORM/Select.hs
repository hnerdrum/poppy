module ORM.Select
  ( OmitSelect (..),
    Picked (..),
    picked,
  )
where

data OmitSelect = OmitSelect
  deriving (Show, Eq)

data Picked a
  = Picked a
  | Skipped
  deriving (Show, Eq)

picked :: Bool -> a -> Picked a
picked True value = Picked value
picked False _ = Skipped
