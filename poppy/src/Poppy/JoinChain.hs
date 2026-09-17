{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.JoinChain
  ( JoinChain,
    JoinedTable (..),
    startJoinChain,
    buildJoinChainFromHasMany,
    addHasManyJoin,
    addBelongsToJoin,
    selectColumns,
    selectFields,
    whereJoin,
    col,
    aliasedField,
    buildJoinQuery,
    runJoinChain,
    rootAlias,
    limitJoin,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Core (Entity (..), Field (..))
import Poppy.Db (Db (..))
import Poppy.Query (buildWhereClause)
import Poppy.Relation (BelongsTo (..), HasMany (..), JoinType (..))
import Poppy.Sql (quoteIdent, quoteQualified)

data JoinedTable = JoinedTable
  { jtTable :: Text,
    jtAlias :: Text,
    jtJoinType :: JoinType,
    jtOnLeft :: Text,
    jtOnRight :: Text
  }
  deriving (Show, Eq)

data JoinChain root = JoinChain
  { jcRoot :: Text,
    jcRootAlias :: Text,
    jcJoins :: [JoinedTable],
    jcSelect :: [Text],
    jcWhere :: [Text],
    jcParams :: [Action],
    jcOrderBy :: [Text],
    jcLimit :: Maybe Int
  }
  deriving (Show)

startJoinChain ::
  forall root.
  (Entity root) =>
  Text ->
  JoinChain root
startJoinChain alias =
  JoinChain
    { jcRoot = tableName @root,
      jcRootAlias = alias,
      jcJoins = [],
      jcSelect = [],
      jcWhere = [],
      jcParams = [],
      jcOrderBy = [quoteQualified alias (fieldColumn (primaryKey @root))],
      jcLimit = Nothing
    }

buildJoinChainFromHasMany ::
  forall parent child key.
  (Entity parent, Entity child) =>
  HasMany parent child key ->
  Text ->
  Text ->
  JoinChain parent
buildJoinChainFromHasMany rel parentAlias childAlias =
  addHasManyJoin rel parentAlias childAlias (startJoinChain @parent parentAlias)

addHasManyJoin ::
  forall root parent child key.
  (Entity child) =>
  HasMany parent child key ->
  Text ->
  Text ->
  JoinChain root ->
  JoinChain root
addHasManyJoin HasMany {joinType, localKey, foreignKey} leftAlias childAlias chain =
  chain
    { jcJoins =
        jcJoins chain
          ++ [ JoinedTable
                 { jtTable = tableName @child,
                   jtAlias = childAlias,
                   jtJoinType = joinType,
                   jtOnLeft = quoteQualified leftAlias (fieldColumn localKey),
                   jtOnRight = quoteQualified childAlias (fieldColumn foreignKey)
                 }
             ],
      jcOrderBy =
        jcOrderBy chain ++ [quoteQualified childAlias (fieldColumn (primaryKey @child))]
    }

addBelongsToJoin ::
  forall root child parent key.
  (Entity parent) =>
  BelongsTo child parent key ->
  Text ->
  Text ->
  JoinChain root ->
  JoinChain root
addBelongsToJoin BelongsTo {joinType, foreignKey, references} childAlias parentAlias chain =
  chain
    { jcJoins =
        jcJoins chain
          ++ [ JoinedTable
                 { jtTable = tableName @parent,
                   jtAlias = parentAlias,
                   jtJoinType = joinType,
                   jtOnLeft = quoteQualified childAlias (fieldColumn foreignKey),
                   jtOnRight = quoteQualified parentAlias (fieldColumn references)
                 }
             ],
      jcOrderBy =
        jcOrderBy chain ++ [quoteQualified parentAlias (fieldColumn (primaryKey @parent))]
    }

selectColumns ::
  [Text] ->
  JoinChain root ->
  JoinChain root
selectColumns cols chain =
  chain {jcSelect = cols}

selectFields ::
  [(Text, [Field table a])] ->
  JoinChain root ->
  JoinChain root
selectFields fieldGroups chain =
  let columns = concatMap (\(alias, fields) -> map (col alias) fields) fieldGroups
   in chain {jcSelect = columns}

whereJoin ::
  Text ->
  [Action] ->
  JoinChain root ->
  JoinChain root
whereJoin condition params chain =
  chain
    { jcWhere = jcWhere chain ++ [condition],
      jcParams = jcParams chain ++ params
    }

limitJoin :: Int -> JoinChain root -> JoinChain root
limitJoin n chain = chain {jcLimit = Just n}

col :: Text -> Field table a -> Text
col alias field = quoteQualified alias (fieldColumn field)

aliasedField :: Maybe Text -> Field table a -> Text
aliasedField maybeAlias field =
  case maybeAlias of
    Nothing -> quoteIdent (fieldColumn field)
    Just alias -> col alias field

joinTypeText :: JoinType -> Text
joinTypeText LeftJoin = "LEFT JOIN"
joinTypeText InnerJoin = "INNER JOIN"
joinTypeText RightJoin = "RIGHT JOIN"

buildJoinClause :: JoinedTable -> Text
buildJoinClause jt =
  joinTypeText (jtJoinType jt)
    <> " "
    <> quoteIdent (jtTable jt)
    <> " "
    <> quoteIdent (jtAlias jt)
    <> " ON "
    <> jtOnLeft jt
    <> " = "
    <> jtOnRight jt

buildJoinQuery :: JoinChain root -> (Query, [Action])
buildJoinQuery jc =
  let rootRef = quoteIdent (jcRoot jc) <> " " <> quoteIdent (jcRootAlias jc)
      selectClause =
        if null (jcSelect jc)
          then "*"
          else Text.intercalate ", " (jcSelect jc)
      joinClauses = map buildJoinClause (jcJoins jc)
      fromClause = " FROM " <> rootRef <> " " <> Text.intercalate " " joinClauses
      whereClause = buildWhereClause (jcWhere jc)
      orderClause = buildOrderClause (jcOrderBy jc)
      limitClause =
        case jcLimit jc of
          Nothing -> ""
          Just n -> " LIMIT " <> Text.pack (show n)
      queryText = "SELECT " <> selectClause <> fromClause <> whereClause <> orderClause <> limitClause
   in (Query (TE.encodeUtf8 queryText), jcParams jc)

buildOrderClause :: [Text] -> Text
buildOrderClause [] = ""
buildOrderClause cols =
  " ORDER BY " <> Text.intercalate ", " (map (<> " ASC") cols)

runJoinChain ::
  forall result root.
  (FromRow result) =>
  JoinChain root ->
  Db [result]
runJoinChain chain = Db (`runJoinChainConn` chain)

runJoinChainConn ::
  forall result root.
  (FromRow result) =>
  Connection ->
  JoinChain root ->
  IO [result]
runJoinChainConn conn chain = do
  let (query, params) = buildJoinQuery chain
  PGSimple.queryWith fromRow conn query params

rootAlias :: JoinChain root -> Text
rootAlias = jcRootAlias
