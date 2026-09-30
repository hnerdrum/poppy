-- | Table and column names default from Haskell names (@Task@ → @task@, @createdAt@ → @created_at@).
--
-- Application Schemas live in a codegen executable and are passed to
-- 'Poppy.Codegen.CLI.generate'. Field types, relations, and uniques are
-- documented in the repository @docs/schema.md@.
module Poppy.Codegen.Schema
  ( Schema (..),
    schema,
    Model (..),
    model,
    table,
    FieldSpec (..),
    FieldType (..),
    FieldDefault (..),
    uuid,
    text,
    int,
    numeric,
    jsonb,
    bool,
    timestamptz,
    enumField,
    pk,
    nullable,
    updatedAt,
    withDefault,
    column,
    (&),
    RelationSpec (..),
    RelationKind (..),
    JoinKind (..),
    hasMany,
    belongsTo,
    ModelInclude (..),
    IncludeTree (..),
    EnumSpec (..),
    EnumVariant (..),
    enum_,
    enumImportFrom,
    variant,
    variantMap,
    UniqueConstraint (..),
    unique_,
    isFullGraphInclude,
    fullGraphIncludeName,
  )
where

import Data.Function ((&))
import Data.List (find)
import Data.Text (Text)
import Poppy.Codegen.IR
  ( EnumSpec (..),
    EnumVariant (..),
    FieldDefault (..),
    FieldSpec (..),
    FieldType (..),
    IncludeTree (..),
    JoinKind (..),
    Model (..),
    ModelInclude (..),
    RelationKind (..),
    RelationSpec (..),
    Schema (..),
    UniqueConstraint (..),
    enumImportFrom,
    enum_,
    nullable,
    pk,
    unique_,
    updatedAt,
    variant,
    variantMap,
  )
import qualified Poppy.Codegen.IR as IR
import Poppy.Codegen.TextUtil (camelToSnake)

-- | Application-side default (@Maybe@ on create) and a drift requirement:
-- the SQL migration must set the matching column @DEFAULT@.
--
-- * 'DefaultUuidV4' → @uuid_generate_v4()@ or @gen_random_uuid()@
-- * 'DefaultNow' → @now()@ or @CURRENT_TIMESTAMP@
withDefault :: FieldDefault -> FieldSpec -> FieldSpec
withDefault = IR.withDefault

-- | Enums, models, and uniques. Models that declare relations also get a full-graph Include.
schema :: [EnumSpec] -> [Model] -> [UniqueConstraint] -> Schema
schema enums models uniques =
  Schema
    { schemaEnums = enums,
      schemaModels = models,
      schemaIncludes =
        [ fullGraphInclude models m
          | m <- models,
            not (null (modelRelations m))
        ],
      schemaUniques = uniques
    }

-- | @model \"Task\" fields relations@. Table name defaults to snake_case of the model name.
model :: Text -> [FieldSpec] -> [RelationSpec] -> Model
model name fields rels =
  Model
    { modelName = name,
      modelTable = camelToSnake name,
      modelFields = fields,
      modelRelations = map bind rels
    }
  where
    bind rel = rel {relFromModel = name}

-- | Override the Postgres table name (@model \"Task\" … & table \"tasks\"@).
table :: Text -> Model -> Model
table name m = m {modelTable = name}

-- | @uuid@ column. Haskell type @UUID@.
uuid :: Text -> FieldSpec
uuid = typedField IR.TyUuid

-- | @text@ column. Haskell type @Text@.
text :: Text -> FieldSpec
text = typedField IR.TyText

-- | @integer@ column. Haskell type @Int@.
int :: Text -> FieldSpec
int = typedField IR.TyInt

-- | @numeric@ column. Haskell type @Scientific@.
numeric :: Text -> FieldSpec
numeric = typedField IR.TyNumeric

-- | @jsonb@ column. Haskell type 'Data.Aeson.Value'.
jsonb :: Text -> FieldSpec
jsonb = typedField IR.TyJsonb

-- | @boolean@ column.
bool :: Text -> FieldSpec
bool = typedField IR.TyBool

-- | @timestamptz@ column. Haskell type @UTCTime@.
timestamptz :: Text -> FieldSpec
timestamptz = typedField IR.TyTimestamptz

-- | Postgres enum declared with 'enum_'.
enumField :: Text -> Text -> FieldSpec
enumField name enumName = typedField (IR.TyEnum enumName) name

typedField :: IR.FieldType -> Text -> FieldSpec
typedField ty name =
  IR.field name ty & column (camelToSnake name)

-- | Override the Postgres column name (@text \"title\" & column \"heading\"@).
column :: Text -> FieldSpec -> FieldSpec
column = IR.column

-- | @hasMany \"authorPosts\" \"Post\" \"authorId\"@: the other table's @authorId@ points at this model's PK.
-- The include record field is the relation name with the owner prefix stripped when present
-- (@authorPosts@ on @Author@ → @posts@).
hasMany ::
  Text ->
  Text ->
  Text ->
  RelationSpec
hasMany name toModel = IR.hasMany name "" toModel "id"

-- | @belongsTo \"author\" \"Author\" \"authorId\"@: this model's @authorId@ points at @Author@'s PK.
belongsTo :: Text -> Text -> Text -> RelationSpec
belongsTo name toModel foreignFld = IR.belongsTo name "" toModel foreignFld "id"

fullGraphInclude :: [Model] -> Model -> ModelInclude
fullGraphInclude models current =
  ModelInclude
    { includeName = fullGraphIncludeName (modelName current),
      includeRootModel = modelName current,
      includeTree = graphTree models (modelName current)
    }

graphTree :: [Model] -> Text -> [IncludeTree]
graphTree models rootName =
  case findModel models rootName of
    Nothing -> []
    Just root -> walk [] root
  where
    walk path current =
      map (edge path current) (modelRelations current)
    edge path current rel =
      let childName = relToModel rel
          nextPath = modelName current : path
       in IncludeTree
            { includeRelation = relName rel,
              includeChildren =
                if childName `elem` nextPath
                  then []
                  else case findModel models childName of
                    Nothing -> []
                    Just child -> walk nextPath child
            }

isFullGraphInclude :: [Model] -> ModelInclude -> Bool
isFullGraphInclude models incl =
  includeName incl == fullGraphIncludeName (includeRootModel incl)
    && includeTree incl == graphTree models (includeRootModel incl)

fullGraphIncludeName :: Text -> Text
fullGraphIncludeName root = root <> "Include"

findModel :: [Model] -> Text -> Maybe Model
findModel models name =
  find ((== name) . modelName) models
