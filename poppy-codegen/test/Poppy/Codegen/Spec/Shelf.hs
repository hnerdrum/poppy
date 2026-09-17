{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Shelf
  ( shelfSchema,
  )
where

import Poppy.Codegen.Schema

shelfSchema :: Schema
shelfSchema =
  schema
    []
    [shelfModel, bookModel, chapterModel, sectionModel, tagModel]
    []

shelfModel :: Model
shelfModel =
  model
    "Shelf"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      text "name"
    ]
    [ hasMany "shelfBooks" "Book" "shelfId",
      hasMany "shelfTags" "Tag" "shelfId"
    ]
    & table "test_shelf"

bookModel :: Model
bookModel =
  model
    "Book"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "shelfId",
      text "title"
    ]
    [hasMany "bookChapters" "Chapter" "bookRef"]
    & table "test_book"

chapterModel :: Model
chapterModel =
  model
    "Chapter"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "bookRef" & column "book_id",
      text "heading"
    ]
    [hasMany "chapterSections" "Section" "chapterRef"]
    & table "test_chapter"

sectionModel :: Model
sectionModel =
  model
    "Section"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "chapterRef" & column "chapter_id",
      text "label"
    ]
    []
    & table "test_section"

tagModel :: Model
tagModel =
  model
    "Tag"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      uuid "shelfId",
      text "label"
    ]
    []
    & table "test_tag"
