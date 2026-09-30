-- | Column picking for generated @*Select@ records. Default is 'OmitSelect' (full row).
module Poppy.Select
  ( OmitSelect (..),
    Picked (..),
    picked,
  )
where

-- | @select_@ default: every column, result is the full row type.
data OmitSelect = OmitSelect
  deriving (Show, Eq)

-- | One column in a partial select.
data Picked a
  = Picked a
  | Skipped
  deriving (Show, Eq)

picked :: Bool -> a -> Picked a
picked True value = Picked value
picked False _ = Skipped
