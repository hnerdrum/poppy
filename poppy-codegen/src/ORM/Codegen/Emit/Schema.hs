{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Emit.Schema
  ( emitModelModule,
    emitEnumModule,
    emitHasMany,
    emitBelongsTo,
    enumImportLine,
  )
where

import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ORM.Codegen.EmitCommon
  ( createTypeName,
    fieldBinder,
    hsType,
    parsePickedName,
    pickedTypeName,
    primaryKeyField,
    rowTypeName,
    selectColumnsFnName,
    selectDefaultName,
    selectTypeName,
    tableTypeName,
    toPickedName,
    updateTypeName,
  )
import ORM.Codegen.IR
import ORM.Codegen.Lookup (lookupField, lookupModel, lookupUniques)
import ORM.Codegen.TextUtil (lowerFirst)

emitModelModule :: Text -> Schema -> Model -> Text
emitModelModule moduleName schema model =
  T.unlines $
    concat
      [ pragmas,
        [ "module " <> moduleName,
          emitExports model,
          "where",
          "",
          imports moduleName schema model,
          "",
          "data " <> tableName_ <> " = " <> tableName_,
          "",
          "type instance PrimaryKeyType " <> tableName_ <> " = " <> pkHsType model,
          "",
          "instance Entity " <> tableName_ <> " where",
          "  tableName = \"" <> modelTable model <> "\"",
          "  primaryKey = " <> fieldBinder model pkField,
          "  tableColumns = [" <> T.intercalate ", " (map colLit (modelFields model)) <> "]"
        ]
        ++ uniqueKeysLines schema model
        ++ [ "",
          emitInsertable model,
          "",
          emitUpdatable model,
          "",
          emitRow model,
          "",
          emitCreate model,
          "",
          emitUpdate model,
          "",
          emitFromRow model,
          "",
          emitSelectTypes model,
          ""
        ],
        concatMap (emitFieldDecl model) (modelFields model),
        map (emitHasMany schema) (hasManyRelations model),
        map (emitBelongsTo schema) (belongsToRelations model)
      ]
  where
    tableName_ = tableTypeName model
    pkField = primaryKeyField model

emitExports :: Model -> Text
emitExports model =
  "  ( "
    <> T.intercalate ",\n    " items
    <> "\n  )"
  where
    items =
      [ tableTypeName model <> " (..)",
        rowTypeName model <> " (..)",
        selectTypeName model <> " (..)",
        pickedTypeName model <> " (..)",
        selectDefaultName model,
        selectColumnsFnName model,
        parsePickedName model,
        toPickedName model,
        createTypeName model <> " (..)",
        updateTypeName model <> " (..)"
      ]
        ++ map (fieldBinder model) (modelFields model)
        ++ map relName (hasManyRelations model)
        ++ map relName (belongsToRelations model)

pragmas :: [Text]
pragmas =
  [ "{-# LANGUAGE AllowAmbiguousTypes #-}",
    "{-# LANGUAGE DuplicateRecordFields #-}",
    "{-# LANGUAGE NoFieldSelectors #-}",
    "{-# LANGUAGE OverloadedRecordDot #-}",
    "{-# LANGUAGE TypeApplications #-}",
    ""
  ]

imports :: Text -> Schema -> Model -> Text
imports moduleName schema model =
  T.intercalate "\n" $
    filter
      (not . T.null)
      [ "import Data.Text (Text)",
        if needsTime model then "import Data.Time (UTCTime)" else "",
        if needsUuid model then "import Data.UUID (UUID)" else "",
        "import ORM.PG (FromRow (..), RowParser, field)",
        "import ORM.Core",
        "import ORM.Select (Picked (..), picked)",
        insertImport model,
        updateImport model,
        relationImport model
      ]
      ++ enumImports moduleName schema model
      ++ relationModelImports moduleName schema model

enumImports :: Text -> Schema -> Model -> [Text]
enumImports moduleName schema model =
  [ enumImportLine (schemaEnumPrefix moduleName) e
    | e <- schemaEnums schema,
      enumName e `elem` usedEnumNames
  ]
  where
    usedEnumNames =
      [ name
        | f <- modelFields model,
          TyEnum name <- [fieldType f]
      ]

schemaEnumPrefix :: Text -> Text
schemaEnumPrefix moduleName =
  case T.breakOnEnd "." moduleName of
    (prefix, _) | not (T.null prefix) -> prefix
    _ -> ""

enumImportLine :: Text -> EnumSpec -> Text
enumImportLine prefix e =
  "import " <> enumImportModule prefix e <> " (" <> enumName e <> ")"

enumImportModule :: Text -> EnumSpec -> Text
enumImportModule prefix e =
  case enumImport e of
    Just imp -> imp
    Nothing -> prefix <> enumName e

enumToStringName :: EnumSpec -> Text
enumToStringName e = lowerFirst (enumName e) <> "ToString"

emitEnumModule :: Text -> EnumSpec -> Text
emitEnumModule moduleName e =
  T.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}",
      "",
      "module " <> moduleName,
      "  ( " <> enumName e <> " (..),",
      "    " <> enumToStringName e,
      "  )",
      "where",
      "",
      "import Data.Maybe (isNothing)",
      "import Data.Text (Text)",
      "import ORM.PG",
      "  ( FromField (..),",
      "    ResultError (ConversionFailed, UnexpectedNull),",
      "    returnError,",
      "    ToField (..),",
      "    toField,",
      "  )",
      "",
      emitEnumData e,
      "",
      emitEnumFromField e,
      "",
      emitEnumToField e,
      "",
      emitEnumToString e
    ]

emitEnumData :: EnumSpec -> Text
emitEnumData e =
  T.unlines
    [ "data " <> enumName e <> " = " <> T.intercalate " | " (map variantName (enumVariants e)),
      "  deriving (Show, Eq)"
    ]

emitEnumFromField :: EnumSpec -> Text
emitEnumFromField e =
  T.unlines $
    [ "instance FromField " <> enumName e <> " where",
      "  fromField f bs",
      "    | isNothing bs = returnError UnexpectedNull f \"\""
    ]
      ++ [ "    | bs == Just \"" <> variantDbStr v <> "\" = pure " <> variantName v
           | v <- enumVariants e
         ]
      ++ ["    | otherwise = returnError ConversionFailed f \"\""]

emitEnumToField :: EnumSpec -> Text
emitEnumToField e =
  T.unlines
    [ "instance ToField " <> enumName e <> " where",
      "  toField = toField . " <> enumToStringName e
    ]

emitEnumToString :: EnumSpec -> Text
emitEnumToString e =
  T.unlines $
    (enumToStringName e <> " :: " <> enumName e <> " -> Text")
      : [ enumToStringName e <> " " <> variantName v <> " = \"" <> variantDbStr v <> "\""
          | v <- enumVariants e
        ]

variantDbStr :: EnumVariant -> Text
variantDbStr v = fromMaybe (lowerFirst (variantName v)) (variantDbValue v)

relationModelImports :: Text -> Schema -> Model -> [Text]
relationModelImports moduleName schema model =
  concatMap hasManyImport (hasManyRelations model)
    ++ concatMap belongsToImport (belongsToRelations model)
  where
    hasManyImport rel =
      let child = lookupModel schema (relToModel rel)
          foreignField = lookupField child (relForeignField rel)
          childMod = siblingModule moduleName child
       in [ "import "
              <> childMod
              <> " ("
              <> tableTypeName child
              <> ", "
              <> fieldBinder child foreignField
              <> ")"
          ]
    belongsToImport rel =
      let parent = lookupModel schema (relToModel rel)
          parentMod = siblingModule moduleName parent
       in [ "import " <> parentMod <> " (" <> tableTypeName parent <> ")",
            "import qualified " <> parentMod <> " as " <> modelName parent
          ]

siblingModule :: Text -> Model -> Text
siblingModule moduleName model =
  case T.breakOnEnd "." moduleName of
    (prefix, _) | not (T.null prefix) -> prefix <> modelName model
    _ -> modelName model

insertImport :: Model -> Text
insertImport model =
  let needsNullable = any fieldNullable (modelFields model)
      needsMaybe = any ((== CreateMaybe) . createKind) (modelFields model)
      parts =
        ["Insertable (..)", "emptyInsert"]
          ++ ["set" | hasRequiredCreateField model]
          ++ ["setMaybe" | needsMaybe]
          ++ ["setNullable" | needsNullable]
   in "import ORM.Insert (" <> T.intercalate ", " parts <> ")"

updateImport :: Model -> Text
updateImport model =
  let needsNullable = any fieldNullable (updateFields model)
      needsMaybe = (not . all fieldNullable) (updateFields model)
      parts =
        ["Updatable (..)", "emptyUpdate"]
          ++ ["setFieldMaybe" | needsMaybe]
          ++ ["setFieldNullable" | needsNullable]
   in "import ORM.Update (" <> T.intercalate ", " parts <> ")"

relationImport :: Model -> Text
relationImport model =
  case (needsHasMany model, needsBelongsTo model) of
    (False, False) -> ""
    (True, False) -> "import ORM.Relation (HasMany (..), JoinType (..))"
    (False, True) -> "import ORM.Relation (BelongsTo (..), JoinType (..))"
    (True, True) -> "import ORM.Relation (HasMany (..), BelongsTo (..), JoinType (..))"

needsTime :: Model -> Bool
needsTime = any ((== TyTimestamptz) . fieldType) . modelFields

needsUuid :: Model -> Bool
needsUuid = any (isUuidType . fieldType) . modelFields
  where
    isUuidType TyUuid = True
    isUuidType _ = False

hasRequiredCreateField :: Model -> Bool
hasRequiredCreateField =
  any (\f -> createKind f == CreateRequired) . modelFields

needsHasMany :: Model -> Bool
needsHasMany = not . null . hasManyRelations

hasManyRelations :: Model -> [RelationSpec]
hasManyRelations =
  filter ((== RelHasMany) . relKind) . modelRelations

needsBelongsTo :: Model -> Bool
needsBelongsTo = not . null . belongsToRelations

belongsToRelations :: Model -> [RelationSpec]
belongsToRelations =
  filter ((== RelBelongsTo) . relKind) . modelRelations

pkHsType :: Model -> Text
pkHsType model = hsType (fieldType (primaryKeyField model))

rowHsType :: FieldSpec -> Text
rowHsType f
  | fieldNullable f = "Maybe " <> hsType (fieldType f)
  | otherwise = hsType (fieldType f)

data CreateKind = CreateMaybe | CreateNullable | CreateRequired
  deriving (Eq)

createKind :: FieldSpec -> CreateKind
createKind f
  | fieldIsPrimaryKey f && isJust (fieldDefault f) = CreateMaybe
  | fieldIsPrimaryKey f = CreateRequired
  | isJust (fieldDefault f) = CreateMaybe
  | fieldNullable f = CreateNullable
  | otherwise = CreateRequired

createHsType :: FieldSpec -> Text
createHsType f = case createKind f of
  CreateMaybe -> "Maybe " <> hsType (fieldType f)
  CreateNullable -> "NullableValue " <> hsType (fieldType f)
  CreateRequired -> hsType (fieldType f)

updateFields :: Model -> [FieldSpec]
updateFields = filter (not . fieldIsPrimaryKey) . modelFields

updateHsType :: FieldSpec -> Text
updateHsType f
  | fieldNullable f = "NullableValue " <> hsType (fieldType f)
  | otherwise = "Maybe " <> hsType (fieldType f)

emitRow :: Model -> Text
emitRow model =
  T.unlines
    [ "data " <> rowTypeName model <> " = " <> rowTypeName model,
      "  { " <> T.intercalate ",\n    " (map rowField (modelFields model)),
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    rowField f = fieldName f <> " :: " <> rowHsType f

emitCreate :: Model -> Text
emitCreate model =
  T.unlines
    [ "data " <> createTypeName model <> " = " <> createTypeName model,
      "  { " <> T.intercalate ",\n    " (map createField (modelFields model)),
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    createField f = fieldName f <> " :: " <> createHsType f

emitUpdate :: Model -> Text
emitUpdate model =
  T.unlines
    [ "data " <> updateTypeName model <> " = " <> updateTypeName model,
      "  { " <> T.intercalate ",\n    " (map updateField (updateFields model)),
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    updateField f = fieldName f <> " :: " <> updateHsType f

emitFromRow :: Model -> Text
emitFromRow model =
  let n = length (modelFields model)
      fields = T.intercalate " <*> " (replicate n "field")
   in T.unlines
        [ "instance FromRow " <> rowTypeName model <> " where",
          "  fromRow = " <> rowTypeName model <> " <$> " <> fields
        ]

colLit :: FieldSpec -> Text
colLit f = "\"" <> fieldColumn f <> "\""

uniqueKeysLines :: Schema -> Model -> [Text]
uniqueKeysLines schema model =
  case lookupUniques schema (modelName model) of
    [] -> []
    uniques ->
      [ "  uniqueKeys = ["
          <> T.intercalate ", " (map emitKey (pkCols : map uniqueCols uniques))
          <> "]"
      ]
  where
    pkCols = [fieldColumn (primaryKeyField model)]
    uniqueCols constraint = map (fieldColumn . lookupField model) (uniqueFields constraint)
    emitKey cols = "[" <> T.intercalate ", " (map (\c -> "\"" <> c <> "\"") cols) <> "]"

pickedFieldHsType :: FieldSpec -> Text
pickedFieldHsType f
  | fieldIsPrimaryKey f = rowHsType f
  | fieldNullable f = "Picked (" <> rowHsType f <> ")"
  | otherwise = "Picked " <> rowHsType f

emitSelectTypes :: Model -> Text
emitSelectTypes model =
  T.intercalate "\n" (map T.strip sections) <> "\n"
  where
    sections =
      [ emitSelectRecord model,
        emitPickedRecord model,
        emitSelectDefault model,
        emitSelectColumnsFn model,
        emitParsePicked model,
        emitToPicked model
      ]

emitSelectRecord :: Model -> Text
emitSelectRecord model =
  T.unlines
    [ "data " <> selectTypeName model <> " = " <> selectTypeName model,
      "  { " <> T.intercalate ",\n    " (map selectField (modelFields model)),
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    selectField f = fieldName f <> " :: Bool"

emitPickedRecord :: Model -> Text
emitPickedRecord model =
  T.unlines
    [ "data " <> pickedTypeName model <> " = " <> pickedTypeName model,
      "  { " <> T.intercalate ",\n    " (map pickedField (modelFields model)),
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    pickedField f = fieldName f <> " :: " <> pickedFieldHsType f

emitSelectDefault :: Model -> Text
emitSelectDefault model =
  T.unlines
    [ selectDefaultName model <> " :: " <> selectTypeName model,
      selectDefaultName model <> " =",
      "  " <> selectTypeName model,
      "    { " <> T.intercalate ",\n      " (map falseField (modelFields model)),
      "    }"
    ]
  where
    falseField f = fieldName f <> " = False"

emitSelectColumnsFn :: Model -> Text
emitSelectColumnsFn model =
  T.unlines
    [ selectColumnsFnName model <> " :: " <> selectTypeName model <> " -> [Text]",
      selectColumnsFnName model <> " select_ =",
      "  fieldColumn " <> fieldBinder model pkSpec,
      "    : concat",
      "      [ " <> T.intercalate "\n      , " (map colGuard others),
      "      ]"
    ]
  where
    pkSpec = primaryKeyField model
    others = filter (not . fieldIsPrimaryKey) (modelFields model)
    colGuard f =
      "[fieldColumn " <> fieldBinder model f <> " | select_." <> fieldName f <> "]"

emitParsePicked :: Model -> Text
emitParsePicked model =
  T.unlines $
    [ parsePickedName model <> " :: " <> selectTypeName model <> " -> RowParser " <> pickedTypeName model,
      parsePickedName model <> " select_ = do",
      "  " <> pkName <> "Val <- field"
    ]
      ++ map parseLine others
      ++ [ "  pure "
             <> pickedTypeName model
             <> " { "
             <> T.intercalate ", " assignFields
             <> " }"
         ]
  where
    pkSpec = primaryKeyField model
    pkName = fieldName pkSpec
    others = filter (not . fieldIsPrimaryKey) (modelFields model)
    parseLine f =
      "  " <> fieldName f <> "Val <- if select_." <> fieldName f <> " then Picked <$> field else pure Skipped"
    assignFields =
      (pkName <> " = " <> pkName <> "Val")
        : [fieldName f <> " = " <> fieldName f <> "Val" | f <- others]

emitToPicked :: Model -> Text
emitToPicked model =
  T.unlines
    [ toPickedName model <> " :: " <> selectTypeName model <> " -> " <> rowTypeName model <> " -> " <> pickedTypeName model,
      toPickedName model <> " select_ row =",
      "  " <> pickedTypeName model,
      "    { " <> T.intercalate ",\n      " assignFields,
      "    }"
    ]
  where
    assignFields = map assign (modelFields model)
    assign f
      | fieldIsPrimaryKey f = fieldName f <> " = row." <> fieldName f
      | otherwise = fieldName f <> " = picked select_." <> fieldName f <> " row." <> fieldName f

emitFieldDecl :: Model -> FieldSpec -> [Text]
emitFieldDecl model f =
  [ binder <> " :: Field " <> tableTypeName model <> " " <> hsType (fieldType f),
    binder <> " = Field \"" <> fieldName f <> "\" \"" <> fieldColumn f <> "\"",
    ""
  ]
  where
    binder = fieldBinder model f

emitInsertable :: Model -> Text
emitInsertable model =
  T.unlines
    [ "instance Insertable " <> tableTypeName model <> " where",
      "  type CreateInput " <> tableTypeName model <> " = " <> createTypeName model,
      "  toInsertBuilder input =",
      "    " <> body
    ]
  where
    fields = modelFields model
    body = foldr wrap ("emptyInsert @" <> tableTypeName model) fields
    wrap f inner = case createKind f of
      CreateNullable ->
        "setNullable " <> fieldBinder model f <> " input." <> fieldName f <> " $\n      " <> inner
      CreateMaybe ->
        "setMaybe " <> fieldBinder model f <> " input." <> fieldName f <> " $\n      " <> inner
      CreateRequired ->
        "set " <> fieldBinder model f <> " input." <> fieldName f <> " $\n      " <> inner

emitUpdatable :: Model -> Text
emitUpdatable model =
  T.unlines
    [ "instance Updatable " <> tableTypeName model <> " where",
      "  type UpdateInput " <> tableTypeName model <> " = " <> updateTypeName model,
      updatedAtLine,
      "  toUpdateBuilder input =",
      "    " <> body
    ]
  where
    fields = updateFields model
    body = foldr wrap ("emptyUpdate @" <> tableTypeName model) fields
    wrap f inner
      | fieldNullable f =
          "setFieldNullable " <> fieldBinder model f <> " input." <> fieldName f <> " $\n      " <> inner
      | otherwise =
          "setFieldMaybe " <> fieldBinder model f <> " input." <> fieldName f <> " $\n      " <> inner
    updatedAtLine =
      case mapMaybe updatedAtBinder (modelFields model) of
        (b : _) -> "  updatedAtField = Just " <> b
        [] -> "  updatedAtField = Nothing"
    updatedAtBinder f
      | fieldUpdatedAt f = Just (fieldBinder model f)
      | otherwise = Nothing

emitHasMany :: Schema -> RelationSpec -> Text
emitHasMany schema rel =
  case relKind rel of
    RelBelongsTo ->
      error "emitHasMany: expected RelHasMany"
    RelHasMany ->
      T.unlines
        [ name <> " :: HasMany " <> parentTable <> " " <> childTable <> " " <> keyTy,
          name <> " =",
          "  HasMany",
          "    { localKey = " <> localBinder <> ",",
          "      foreignKey = " <> foreignBinder <> ",",
          "      joinType = " <> joinTy,
          "    }"
        ]
  where
    name = relName rel
    parent = lookupModel schema (relFromModel rel)
    child = lookupModel schema (relToModel rel)
    parentTable = tableTypeName parent
    childTable = tableTypeName child
    localField = lookupField parent (relLocalField rel)
    foreignField = lookupField child (relForeignField rel)
    localBinder = fieldBinder parent localField
    foreignBinder = fieldBinder child foreignField
    keyTy = hsType (fieldType localField)
    joinTy = emitJoinType (relJoin rel)

emitBelongsTo :: Schema -> RelationSpec -> Text
emitBelongsTo schema rel =
  case relKind rel of
    RelHasMany ->
      error "emitBelongsTo: expected RelBelongsTo"
    RelBelongsTo ->
      T.unlines
        [ name <> " :: BelongsTo " <> childTable <> " " <> parentTable <> " " <> keyTy,
          name <> " =",
          "  BelongsTo",
          "    { foreignKey = " <> foreignBinder <> ",",
          "      references = " <> referencesBinder <> ",",
          "      joinType = " <> joinTy,
          "    }"
        ]
  where
    name = relName rel
    child = lookupModel schema (relFromModel rel)
    parent = lookupModel schema (relToModel rel)
    childTable = tableTypeName child
    parentTable = tableTypeName parent
    -- IR: relForeignField = FK on child; relLocalField = referenced field on parent
    foreignField = lookupField child (relForeignField rel)
    referencedField = lookupField parent (relLocalField rel)
    foreignBinder = fieldBinder child foreignField
    referencesBinder =
      modelName parent <> "." <> fieldBinder parent referencedField
    keyTy = hsType (fieldType foreignField)
    joinTy = emitJoinType (relJoin rel)

emitJoinType :: JoinKind -> Text
emitJoinType JoinLeft = "LeftJoin"
emitJoinType JoinInner = "InnerJoin"
emitJoinType JoinRight = "RightJoin"
