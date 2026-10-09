{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Author
  ( authorSchema,
    authorModel,
    postModel,
  )
where

import Poppy.Codegen.Schema

authorSchema :: Schema
authorSchema =
  schema
    [enum_ "PostStatus" [variant "Draft", variant "Published"]]
    [authorModel, postModel]
    []

authorModel :: Model
authorModel =
  model
    "Author"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      text "name"
    ]
    [hasMany "posts" "Post" "authorId"]
    & table "test_author"

postModel :: Model
postModel =
  model
    "Post"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "authorId",
      text "title",
      enumField "status" "PostStatus"
    ]
    [belongsTo "author" "Author" "authorId"]
    & table "test_post"
