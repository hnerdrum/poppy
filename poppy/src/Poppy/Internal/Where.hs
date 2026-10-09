{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Predicates for query records and include edges.
--
-- On a generated query record, @where_@ filters the root model. On 'load'
-- or 'loadWith', @where_@ filters that included relation.
module Poppy.Internal.Where
  ( Where,
    eq,
    neq,
    gt,
    gte,
    lt,
    lte,
    in_,
    contains,
    isNull,
    and_,
    or_,
    not_,
    compileWhere,
    equalityColumns,
  )
where

import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (In (..))
import Poppy.Internal.Core (Field (..))
import Poppy.Internal.Sql (quoteIdent)

-- | Predicate on one table: the root of a query record, or the child of an include edge.
data Where table
  = WhereCmp Text Text Action
  | WhereIn Text Action
  | WhereNull Text
  | WhereContains Text Action
  | WhereAnd (Where table) (Where table)
  | WhereOr (Where table) (Where table)
  | WhereNot (Where table)
  | WhereFalse

instance Eq (Where table) where
  WhereCmp c1 op1 v1 == WhereCmp c2 op2 v2 =
    c1 == c2 && op1 == op2 && sameAction v1 v2
  WhereIn c1 v1 == WhereIn c2 v2 = c1 == c2 && sameAction v1 v2
  WhereNull c1 == WhereNull c2 = c1 == c2
  WhereContains c1 v1 == WhereContains c2 v2 = c1 == c2 && sameAction v1 v2
  WhereAnd a1 b1 == WhereAnd a2 b2 = a1 == a2 && b1 == b2
  WhereOr a1 b1 == WhereOr a2 b2 = a1 == a2 && b1 == b2
  WhereNot a == WhereNot b = a == b
  WhereFalse == WhereFalse = True
  _ == _ = False

instance Show (Where table) where
  show where_ = unpack (fst (compileWhere where_))

sameAction :: Action -> Action -> Bool
sameAction a b = show a == show b

infixr 3 `and_`

infixr 2 `or_`

-- | @=@
eq :: (ToField a) => Field table a -> a -> Where table
eq = cmp "="

-- | @<>@
neq :: (ToField a) => Field table a -> a -> Where table
neq = cmp "<>"

-- | @>@
gt :: (ToField a) => Field table a -> a -> Where table
gt = cmp ">"

-- | @>=@
gte :: (ToField a) => Field table a -> a -> Where table
gte = cmp ">="

-- | @<@
lt :: (ToField a) => Field table a -> a -> Where table
lt = cmp "<"

-- | @<=@
lte :: (ToField a) => Field table a -> a -> Where table
lte = cmp "<="

-- | @IN (…)@. Empty list is false.
in_ :: (ToField a) => Field table a -> [a] -> Where table
in_ _ [] = WhereFalse
in_ field values = WhereIn (fieldColumn field) (toField (In values))

-- | Case-insensitive substring match (@POSITION@ of the needle in the column).
contains :: Field table Text -> Text -> Where table
contains field value =
  WhereContains (fieldColumn field) (toField value)

-- | @IS NULL@
isNull :: Field table a -> Where table
isNull field = WhereNull (fieldColumn field)

-- | @AND@
and_ :: Where table -> Where table -> Where table
and_ = WhereAnd

-- | @OR@
or_ :: Where table -> Where table -> Where table
or_ = WhereOr

-- | @NOT@
not_ :: Where table -> Where table
not_ = WhereNot

cmp :: (ToField a) => Text -> Field table a -> a -> Where table
cmp op field value =
  WhereCmp (fieldColumn field) op (toField value)

-- | Column names of a conjunction of equalities. 'Nothing' if the predicate
-- uses anything other than 'eq' combined with 'and_'.
equalityColumns :: Where table -> Maybe [Text]
equalityColumns = \case
  WhereCmp col "=" _ -> Just [col]
  WhereAnd left right -> (++) <$> equalityColumns left <*> equalityColumns right
  _ -> Nothing

compileWhere :: Where table -> (Text, [Action])
compileWhere = \case
  WhereCmp col op val -> (quoteIdent col <> " " <> op <> " ?", [val])
  WhereIn col val -> (quoteIdent col <> " IN ?", [val])
  WhereNull col -> (quoteIdent col <> " IS NULL", [])
  WhereContains col val ->
    ("POSITION(LOWER(?) IN LOWER(" <> quoteIdent col <> ")) > 0", [val])
  WhereAnd left right -> compileBin "AND" left right
  WhereOr left right -> compileBin "OR" left right
  WhereNot inner ->
    let (sql, params) = compileWhere inner
     in ("NOT (" <> sql <> ")", params)
  WhereFalse -> ("FALSE", [])

compileBin :: Text -> Where table -> Where table -> (Text, [Action])
compileBin op left right =
  let (leftSql, leftParams) = compileWhere left
      (rightSql, rightParams) = compileWhere right
   in ("(" <> leftSql <> ") " <> op <> " (" <> rightSql <> ")", leftParams ++ rightParams)
