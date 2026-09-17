{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Flag
  ( flagModel,
    flagSchema,
  )
where

import Poppy.Codegen.Schema

flagSchema :: Schema
flagSchema =
  schema [] [flagModel] []

flagModel :: Model
flagModel =
  model
    "Flag"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      bool "active"
    ]
    []
