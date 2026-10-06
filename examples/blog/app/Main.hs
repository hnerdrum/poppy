module Main (main) where

import Poppy (applyMigrations, closePool, connect, load, runDb)
import Schema.ArticleStatus (ArticleStatus (..))
import Schema.Author (AuthorRow (..))
import qualified Schema.Client.Author as Author
import Schema.Include.Author (AuthorInclude (..), AuthorWith (..))
import Schema.Post (PostRow)
import System.Environment (getEnv)

main :: IO ()
main = do
  url <- getEnv "DATABASE_URL"
  pool <- connect url
  applied <- applyMigrations pool "migrations"
  case applied of
    Left err -> print err
    Right _ -> do
      created <-
        runDb pool $
          Author.createNested
            (AuthorInclude {posts = load})
            Author.AuthorWriteCreate
              { root = Author.AuthorCreate {id = Nothing, name = "Ada"},
                posts =
                  Author.Set
                    [ Author.PostNestedCreate {id = Nothing, title = "Notes", status = Draft},
                      Author.PostNestedCreate {id = Nothing, title = "Essay", status = Published}
                    ]
              }
      case created of
        Left err -> print err
        Right loaded -> do
          putStrLn $ "author: " <> show loaded.author.name
          putStrLn $ "posts: " <> show (loaded.posts :: [PostRow])
          listed <-
            runDb
              pool
              (Author.findMany Author.emptyQuery {Author.include_ = AuthorInclude {posts = load}})
          putStrLn $ "authors with posts: " <> show (length listed)
  closePool pool
