module Main (main) where

import Data.UUID (UUID)
import Poppy (Db, DbPool, ORMError, applyMigrations, closePool, connect, runDb)
import Poppy.Where (eq)
import qualified Schema.Client.Task as Task
import Schema.Task (TaskCreate (..), TaskRow (..), taskDone, taskId)
import System.Environment (getEnv)

listOpenTasks :: Db [TaskRow]
listOpenTasks =
  Task.findMany
    Task.emptyQuery {Task.where_ = Just (eq taskDone False)}

getTask :: UUID -> Db (Either ORMError TaskRow)
getTask taskKey =
  Task.findUniqueOrFail
    Task.emptyQuery {Task.where_ = Just (eq taskId taskKey)}

main :: IO ()
main = do
  url <- getEnv "DATABASE_URL"
  pool <- connect url
  applied <- applyMigrations pool "migrations"
  case applied of
    Left err -> print err
    Right _ -> runQueries pool
  closePool pool

runQueries :: DbPool -> IO ()
runQueries pool = do
  created <-
    runDb pool $
      Task.create
        TaskCreate
          { id = Nothing,
            createdAt = Nothing,
            updatedAt = Nothing,
            title = "Try Poppy",
            done = False
          }
  case created of
    Left err -> print err
    Right task -> do
      open <- runDb pool listOpenTasks
      putStrLn $ "open tasks: " <> show (length open)
      found <- runDb pool (getTask task.id)
      case found of
        Left err -> print err
        Right row -> putStrLn $ "loaded: " <> show row.title
