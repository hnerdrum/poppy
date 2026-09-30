-- | Schema value types. Application code builds Schemas with "Poppy.Codegen.Schema", not this module.
module Poppy.Codegen.IR
  ( Schema (..),
    Model (..),
    EnumSpec (..),
    EnumVariant (..),
    FieldSpec (..),
    FieldType (..),
    FieldDefault (..),
    RelationSpec (..),
    RelationKind (..),
    JoinKind (..),
    ModelInclude (..),
    IncludeTree (..),
    UniqueConstraint (..),
    field,
    uuid,
    text,
    int,
    numeric,
    jsonb,
    timestamptz,
    bool,
    enumField,
    pk,
    nullable,
    updatedAt,
    withDefault,
    column,
    enum_,
    enumImportFrom,
    variant,
    variantMap,
    hasMany,
    belongsTo,
    unique_,
  )
where

import Data.Text (Text)

-- | Enums, models, generated includes, and uniques.
data Schema = Schema
  { schemaEnums :: [EnumSpec],
    schemaModels :: [Model],
    schemaIncludes :: [ModelInclude],
    schemaUniques :: [UniqueConstraint]
  }
  deriving (Show, Eq)

-- | Unique constraint: model name plus field names (not columns).
data UniqueConstraint = UniqueConstraint
  { uniqueModel :: Text,
    uniqueFields :: [Text]
  }
  deriving (Show, Eq)

-- | Postgres enum in the Schema.
data EnumSpec = EnumSpec
  { enumName :: Text,
    enumVariants :: [EnumVariant],
    enumImport :: Maybe Text
  }
  deriving (Show, Eq)

-- | Haskell constructor and optional distinct Postgres label.
data EnumVariant = EnumVariant
  { variantName :: Text,
    variantDbValue :: Maybe Text
  }
  deriving (Show, Eq)

-- | One table: fields and relations.
data Model = Model
  { modelName :: Text,
    modelTable :: Text,
    modelFields :: [FieldSpec],
    modelRelations :: [RelationSpec]
  }
  deriving (Show, Eq)

-- | Column types Codegen emits and drift-checks.
data FieldType
  = TyText
  | TyUuid
  | TyInt
  | TyNumeric
  | TyJsonb
  | TyTimestamptz
  | TyBool
  | TyEnum Text
  deriving (Show, Eq)

-- | Application-side default; the SQL column must have a matching @DEFAULT@.
data FieldDefault
  = DefaultUuidV4
  | DefaultNow
  deriving (Show, Eq)

-- | One column on a model.
data FieldSpec = FieldSpec
  { fieldName :: Text,
    fieldColumn :: Text,
    fieldType :: FieldType,
    fieldNullable :: Bool,
    fieldIsPrimaryKey :: Bool,
    fieldDefault :: Maybe FieldDefault,
    fieldUpdatedAt :: Bool
  }
  deriving (Show, Eq)

-- | @RelHasMany@ or @RelBelongsTo@.
data RelationKind = RelHasMany | RelBelongsTo
  deriving (Show, Eq)

-- | @LEFT@ / @INNER@ / @RIGHT@ for ad-hoc join emission.
data JoinKind = JoinLeft | JoinInner | JoinRight
  deriving (Show, Eq)

-- | @hasMany@ or @belongsTo@ edge on a model.
data RelationSpec = RelationSpec
  { relName :: Text,
    relKind :: RelationKind,
    relFromModel :: Text,
    relToModel :: Text,
    relLocalField :: Text,
    relForeignField :: Text,
    relJoin :: JoinKind
  }
  deriving (Show, Eq)

-- | Named include tree rooted at a model (full-graph includes are generated).
data ModelInclude = ModelInclude
  { includeName :: Text,
    includeRootModel :: Text,
    includeTree :: [IncludeTree]
  }
  deriving (Show, Eq)

-- | One edge in an include tree, with optional nested children.
data IncludeTree = IncludeTree
  { includeRelation :: Text,
    includeChildren :: [IncludeTree]
  }
  deriving (Show, Eq)

field :: Text -> FieldType -> FieldSpec
field name ty =
  FieldSpec
    { fieldName = name,
      fieldColumn = name,
      fieldType = ty,
      fieldNullable = False,
      fieldIsPrimaryKey = False,
      fieldDefault = Nothing,
      fieldUpdatedAt = False
    }

uuid :: Text -> FieldSpec
uuid name = field name TyUuid

text :: Text -> FieldSpec
text name = field name TyText

int :: Text -> FieldSpec
int name = field name TyInt

numeric :: Text -> FieldSpec
numeric name = field name TyNumeric

jsonb :: Text -> FieldSpec
jsonb name = field name TyJsonb

timestamptz :: Text -> FieldSpec
timestamptz name = field name TyTimestamptz

bool :: Text -> FieldSpec
bool name = field name TyBool

enumField :: Text -> Text -> FieldSpec
enumField name enumName = field name (TyEnum enumName)

-- | Exactly one primary key per model.
pk :: FieldSpec -> FieldSpec
pk f = f {fieldIsPrimaryKey = True}

-- | @Maybe@ on the row; create/update use 'Poppy.Core.NullableValue'.
nullable :: FieldSpec -> FieldSpec
nullable f = f {fieldNullable = True}

-- | Client writes set this column to now.
updatedAt :: FieldSpec -> FieldSpec
updatedAt f = f {fieldUpdatedAt = True}

withDefault :: FieldDefault -> FieldSpec -> FieldSpec
withDefault d f = f {fieldDefault = Just d}

column :: Text -> FieldSpec -> FieldSpec
column col f = f {fieldColumn = col}

-- | Postgres enum. Labels default to the Haskell constructor names.
enum_ :: Text -> [EnumVariant] -> EnumSpec
enum_ name variants =
  EnumSpec {enumName = name, enumVariants = variants, enumImport = Nothing}

-- | Skip generating the sum type; import this module instead.
enumImportFrom :: Text -> EnumSpec -> EnumSpec
enumImportFrom moduleName e = e {enumImport = Just moduleName}

-- | Enum constructor; Postgres label is the same spelling.
variant :: Text -> EnumVariant
variant name = EnumVariant {variantName = name, variantDbValue = Nothing}

-- | Enum constructor with a different Postgres label.
variantMap :: Text -> Text -> EnumVariant
variantMap name dbValue =
  EnumVariant {variantName = name, variantDbValue = Just dbValue}

hasMany :: Text -> Text -> Text -> Text -> Text -> RelationSpec
hasMany name fromModel toModel localFld foreignFld =
  RelationSpec
    { relName = name,
      relKind = RelHasMany,
      relFromModel = fromModel,
      relToModel = toModel,
      relLocalField = localFld,
      relForeignField = foreignFld,
      relJoin = JoinLeft
    }

belongsTo :: Text -> Text -> Text -> Text -> Text -> RelationSpec
belongsTo name fromModel toModel foreignFld referencedFld =
  RelationSpec
    { relName = name,
      relKind = RelBelongsTo,
      relFromModel = fromModel,
      relToModel = toModel,
      relLocalField = referencedFld,
      relForeignField = foreignFld,
      relJoin = JoinLeft
    }

-- | Unique constraint (field names, not columns). @findUnique@ and @upsert@ conflict use these.
unique_ :: Text -> [Text] -> UniqueConstraint
unique_ model fields = UniqueConstraint {uniqueModel = model, uniqueFields = fields}
