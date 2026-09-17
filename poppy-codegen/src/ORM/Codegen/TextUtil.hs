module ORM.Codegen.TextUtil
  ( lowerFirst,
    upperFirst,
    camelToSnake,
  )
where

import Data.Char (isUpper, toLower, toUpper)
import Data.Text (Text)
import qualified Data.Text as T

lowerFirst :: Text -> Text
lowerFirst t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (toLower c) rest

upperFirst :: Text -> Text
upperFirst t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (toUpper c) rest

camelToSnake :: Text -> Text
camelToSnake =
  T.dropWhile (== '_') . T.concatMap step
  where
    step c
      | isUpper c = T.pack ['_', toLower c]
      | otherwise = T.singleton c
