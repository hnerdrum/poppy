module Main (main) where

import Poppy (applyMigrations, closePool, connect, load, runDb)
import Schema.ArticleStatus (ArticleStatus (..))
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
          Author.create
            Author.AuthorCreate
              { id = Nothing,
                name = "Ada",
                posts =
                  [ Author.CreatePost {id = Nothing, title = "Notes", status = Draft},
                    Author.CreatePost {id = Nothing, title = "Essay", status = Published}
                  ]
              }
      case created of
        Left err -> print err
        Right author -> do
          putStrLn $ "author: " <> show author.name
          listed <-
            runDb
              pool
              (Author.findMany Author.emptyQuery {Author.include_ = AuthorInclude {posts = load}})
          putStrLn $ "authors with posts: " <> show (length listed)
          case listed of
            (one : _) ->
              let posts = one.posts :: [PostRow]
               in putStrLn $ "posts: " <> show posts
            [] -> putStrLn "posts: []"
  closePool pool
