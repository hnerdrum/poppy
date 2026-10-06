{-# LANGUAGE OverloadedStrings #-}

module BlogSchema
  ( blogSchema,
  )
where

import Poppy.Codegen.Schema

blogSchema :: Schema
blogSchema =
  schema
    [enum_ "ArticleStatus" [variant "Draft", variant "Published"]]
    [authorModel, postModel]
    [unique_ "Post" ["title"]]

authorModel :: Model
authorModel =
  model
    "Author"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      text "name"
    ]
    [hasMany "posts" "Post" "authorId"]

postModel :: Model
postModel =
  model
    "Post"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "authorId",
      text "title",
      enumField "status" "ArticleStatus"
    ]
    []
