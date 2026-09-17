{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Example
  ( taskModel,
    exampleSchema,
  )
where

import Poppy.Codegen.Schema

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
