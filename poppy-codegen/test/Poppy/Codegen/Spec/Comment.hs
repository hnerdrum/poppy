{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Comment
  ( commentSchema,
  )
where

import Poppy.Codegen.Schema

-- A comment can include its replies, and each reply can include its own.
commentSchema :: Schema
commentSchema =
  schema
    []
    [ commentModel
    ]
    []

commentModel :: Model
commentModel =
  model
    "Comment"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "parentId" & nullable,
      text "body"
    ]
    [hasMany "replies" "Comment" "parentId"]
    & table "test_comment"
