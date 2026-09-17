module ORM.ErrorsSpec
  ( errorsSpec,
  )
where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (SqlError (..))
import ORM.Errors (DatabaseErrorInfo (..), ORMError (..))
import ORM.Sql (fromSqlError, quoteIdent)
import Test.Hspec (Spec, describe, it, shouldBe)

errorsSpec :: Spec
errorsSpec = do
  describe "ORM.Sql.fromSqlError" $ do
    it "maps unique violations to UniqueViolation" $ do
      fromSqlError sampleUniqueError
        `shouldBe` UniqueViolation "duplicate key value violates unique constraint"

    it "maps foreign key violations to ForeignKeyViolation" $ do
      fromSqlError sampleFkError
        `shouldBe` ForeignKeyViolation "insert or update on table violates foreign key constraint"

    it "maps not-null violations to NotNullViolation" $ do
      fromSqlError sampleNotNullError
        `shouldBe` NotNullViolation "null value in column violates not-null constraint"

    it "maps other sql errors to DatabaseError" $ do
      fromSqlError sampleOtherError
        `shouldBe` DatabaseError
          DatabaseErrorInfo
            { sqlState = "08006",
              message = "connection failure",
              detail = ""
            }

  describe "ORM.Sql.quoteIdent" $ do
    it "double-quotes identifiers" $
      quoteIdent "recipe" `shouldBe` "\"recipe\""

    it "escapes embedded quotes" $
      quoteIdent "a\"b" `shouldBe` "\"a\"\"b\""

sampleUniqueError :: SqlError
sampleUniqueError =
  sampleSqlError "23505" "duplicate key value violates unique constraint"

sampleFkError :: SqlError
sampleFkError =
  sampleSqlError "23503" "insert or update on table violates foreign key constraint"

sampleNotNullError :: SqlError
sampleNotNullError =
  sampleSqlError "23502" "null value in column violates not-null constraint"

sampleOtherError :: SqlError
sampleOtherError =
  sampleSqlError "08006" "connection failure"

sampleSqlError :: ByteString -> ByteString -> SqlError
sampleSqlError state msg =
  SqlError
    { sqlState = state,
      sqlExecStatus = toEnum 0,
      sqlErrorMsg = msg,
      sqlErrorDetail = "",
      sqlErrorHint = ""
    }
