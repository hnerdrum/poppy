module Main (main) where

import Poppy (applyMigrations, closePool, connect, runDb)
import Schema.ArticleStatus (ArticleStatus (..))
import Schema.AuthorInclude (AuthorWithPosts (..))
import qualified Schema.Client.Author as Author
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
            Author.withPosts
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
        Right nested -> do
          let loaded = nested :: AuthorWithPosts
          putStrLn $ "author: " <> show loaded.author.name
          putStrLn $ "posts: " <> show (loaded.posts :: [PostRow])
          listed <-
            runDb
              pool
              (Author.findMany Author.emptyQuery {Author.include_ = Author.withPosts})
          putStrLn $ "authors with posts: " <> show (length (listed :: [AuthorWithPosts]))
  closePool pool
