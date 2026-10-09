{-# LANGUAGE OverloadedStrings #-}

module Schema.PostStatus
  ( PostStatus (..),
    postStatusToString
  )
where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Poppy.Internal.Generated
  ( FromField (..),
    ResultError (ConversionFailed, UnexpectedNull),
    returnError,
    ToField (..),
    toField,
  )

data PostStatus = Draft | Published
  deriving (Show, Eq)


instance FromField PostStatus where
  fromField f bs
    | isNothing bs = returnError UnexpectedNull f ""
    | bs == Just "draft" = pure Draft
    | bs == Just "published" = pure Published
    | otherwise = returnError ConversionFailed f ""


instance ToField PostStatus where
  toField = toField . postStatusToString


postStatusToString :: PostStatus -> Text
postStatusToString Draft = "draft"
postStatusToString Published = "published"

