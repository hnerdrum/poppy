{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Editor
  ( editorSchema,
  )
where

import Poppy.Codegen.Schema

-- Two hasMany edges from one model to the same model. The names are the
-- include fields, so both relations compile.
editorSchema :: Schema
editorSchema =
  schema
    []
    [editorModel, articleModel]
    []

editorModel :: Model
editorModel =
  model
    "Editor"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      text "name"
    ]
    [ hasMany "writtenPosts" "Article" "authorId",
      hasMany "editedPosts" "Article" "authorId"
    ]
    & table "test_editor"

articleModel :: Model
articleModel =
  model
    "Article"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "authorId",
      text "title"
    ]
    []
    & table "test_article"
