module Support.Assert
  ( assertRight,
    assertJust,
  )
where

assertRight :: Show e => Either e a -> IO a
assertRight (Left err) = fail (show err)
assertRight (Right x) = pure x

assertJust :: Maybe a -> IO a
assertJust Nothing = fail "expected Just"
assertJust (Just x) = pure x
