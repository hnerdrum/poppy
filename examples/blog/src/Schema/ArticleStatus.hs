{-# LANGUAGE OverloadedStrings #-}

module Schema.ArticleStatus
  ( ArticleStatus (..),
    articleStatusToString
  )
where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Poppy.PG
  ( FromField (..),
    ResultError (ConversionFailed, UnexpectedNull),
    returnError,
    ToField (..),
    toField,
  )

data ArticleStatus = Draft | Published
  deriving (Show, Eq)


instance FromField ArticleStatus where
  fromField f bs
    | isNothing bs = returnError UnexpectedNull f ""
    | bs == Just "draft" = pure Draft
    | bs == Just "published" = pure Published
    | otherwise = returnError ConversionFailed f ""


instance ToField ArticleStatus where
  toField = toField . articleStatusToString


articleStatusToString :: ArticleStatus -> Text
articleStatusToString Draft = "draft"
articleStatusToString Published = "published"

