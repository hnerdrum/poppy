module ORM.Group
  ( groupByKey,
  )
where

import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map

-- | Group rows by key, preserving first-seen key order and row order within
-- each group. Unlike a 'Map'-ordered group, this matches SELECT order so
-- nested includes stay stable when the join is ordered.
groupByKey :: (Ord k) => (a -> k) -> [a] -> [[a]]
groupByKey keyFn rows =
  map snd . sortOn fst . Map.elems $ foldl' add Map.empty rows
  where
    add acc row =
      Map.alter (upsert (Map.size acc) row) (keyFn row) acc
    upsert idx row Nothing = Just (idx, [row])
    upsert _ row (Just (idx, xs)) = Just (idx, xs ++ [row])
