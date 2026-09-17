module ORM.Codegen.IR
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

data Schema = Schema
  { schemaEnums :: [EnumSpec],
    schemaModels :: [Model],
    schemaIncludes :: [ModelInclude],
    schemaUniques :: [UniqueConstraint]
  }
  deriving (Show, Eq)

data UniqueConstraint = UniqueConstraint
  { uniqueModel :: Text,
    uniqueFields :: [Text]
  }
  deriving (Show, Eq)

data EnumSpec = EnumSpec
  { enumName :: Text,
    enumVariants :: [EnumVariant],
    enumImport :: Maybe Text
  }
  deriving (Show, Eq)

data EnumVariant = EnumVariant
  { variantName :: Text,
    variantDbValue :: Maybe Text
  }
  deriving (Show, Eq)

data Model = Model
  { modelName :: Text,
    modelTable :: Text,
    modelFields :: [FieldSpec],
    modelRelations :: [RelationSpec]
  }
  deriving (Show, Eq)

data FieldType
  = TyText
  | TyUuid
  | TyInt
  | TyTimestamptz
  | TyBool
  | TyEnum Text
  deriving (Show, Eq)

data FieldDefault
  = DefaultUuidV4
  | DefaultNow
  deriving (Show, Eq)

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

data RelationKind = RelHasMany | RelBelongsTo
  deriving (Show, Eq)

data JoinKind = JoinLeft | JoinInner | JoinRight
  deriving (Show, Eq)

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

data ModelInclude = ModelInclude
  { includeName :: Text,
    includeRootModel :: Text,
    includeTree :: [IncludeTree]
  }
  deriving (Show, Eq)

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

timestamptz :: Text -> FieldSpec
timestamptz name = field name TyTimestamptz

bool :: Text -> FieldSpec
bool name = field name TyBool

enumField :: Text -> Text -> FieldSpec
enumField name enumName = field name (TyEnum enumName)

pk :: FieldSpec -> FieldSpec
pk f = f {fieldIsPrimaryKey = True}

nullable :: FieldSpec -> FieldSpec
nullable f = f {fieldNullable = True}

updatedAt :: FieldSpec -> FieldSpec
updatedAt f = f {fieldUpdatedAt = True}

withDefault :: FieldDefault -> FieldSpec -> FieldSpec
withDefault d f = f {fieldDefault = Just d}

column :: Text -> FieldSpec -> FieldSpec
column col f = f {fieldColumn = col}

enum_ :: Text -> [EnumVariant] -> EnumSpec
enum_ name variants =
  EnumSpec {enumName = name, enumVariants = variants, enumImport = Nothing}

enumImportFrom :: Text -> EnumSpec -> EnumSpec
enumImportFrom moduleName e = e {enumImport = Just moduleName}

variant :: Text -> EnumVariant
variant name = EnumVariant {variantName = name, variantDbValue = Nothing}

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

unique_ :: Text -> [Text] -> UniqueConstraint
unique_ model fields = UniqueConstraint {uniqueModel = model, uniqueFields = fields}
