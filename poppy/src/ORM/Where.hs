{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ORM.Where
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

import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (In (..))
import ORM.Core (Field (..))
import ORM.Sql (quoteIdent)

data Where table
  = WhereCmp Text Text Action
  | WhereIn Text Action
  | WhereNull Text
  | WhereContains Text Action
  | WhereAnd (Where table) (Where table)
  | WhereOr (Where table) (Where table)
  | WhereNot (Where table)
  | WhereFalse

infixr 3 `and_`

infixr 2 `or_`

eq :: (ToField a) => Field table a -> a -> Where table
eq = cmp "="

neq :: (ToField a) => Field table a -> a -> Where table
neq = cmp "<>"

gt :: (ToField a) => Field table a -> a -> Where table
gt = cmp ">"

gte :: (ToField a) => Field table a -> a -> Where table
gte = cmp ">="

lt :: (ToField a) => Field table a -> a -> Where table
lt = cmp "<"

lte :: (ToField a) => Field table a -> a -> Where table
lte = cmp "<="

in_ :: (ToField a) => Field table a -> [a] -> Where table
in_ _ [] = WhereFalse
in_ field values = WhereIn (fieldColumn field) (toField (In values))

contains :: Field table Text -> Text -> Where table
contains field value =
  WhereContains (fieldColumn field) (toField value)

isNull :: Field table a -> Where table
isNull field = WhereNull (fieldColumn field)

and_ :: Where table -> Where table -> Where table
and_ = WhereAnd

or_ :: Where table -> Where table -> Where table
or_ = WhereOr

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
