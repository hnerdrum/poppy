{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Widget
  ( widgetModel,
    widgetSchema,
  )
where

import Poppy.Codegen.Schema

widgetSchema :: Schema
widgetSchema =
  schema [] [widgetModel] []

widgetModel :: Model
widgetModel =
  model
    "Widget"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      timestamptz "createdAt" & withDefault DefaultNow,
      timestamptz "updatedAt" & withDefault DefaultNow & updatedAt,
      text "name",
      text "description" & nullable
    ]
    []
    & table "test_widget"
