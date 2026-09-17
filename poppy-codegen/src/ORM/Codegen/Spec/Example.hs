{-# LANGUAGE OverloadedStrings #-}

-- | Canonical Schema used in the ORM README and Codegen validate/emit tests.
module ORM.Codegen.Spec.Example
  ( taskModel,
    exampleSchema,
  )
where

import ORM.Codegen.Schema

exampleSchema :: Schema
exampleSchema =
  schema [] [taskModel] []

taskModel :: Model
taskModel =
  model
    "Task"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      timestamptz "createdAt" & withDefault DefaultNow,
      timestamptz "updatedAt" & withDefault DefaultNow & updatedAt,
      text "title",
      bool "done"
    ]
    []
