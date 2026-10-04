{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module IncludeSpike.Examples
  ( Html,
    renderPosts,
    renderedPosts,
    skippedAuthor,
    postsOnly,
    postsAndDrafts,
    nestedShelves,
    requiredAuthor,
    nullableReviewer,
    loadedChapters,
  )
where

import GHC.Records (HasField)
import IncludeSpike.Author
  ( AuthorInclude (..),
    AuthorQuery (..),
    AuthorWith (..),
    PostInclude (..),
    PostQuery (..),
    PostWith,
    findManyPosts,
  )
import qualified IncludeSpike.Author as Author
import IncludeSpike.Rows (AuthorRow (..), ChapterRow, PostRow)
import IncludeSpike.Runtime (Load (..), Skip (..), load, loadWith, skip, skipped)
import IncludeSpike.Shelf
  ( BookInclude (..),
    ShelfInclude (..),
    ShelfQuery (..),
    ShelfWith,
  )
import qualified IncludeSpike.Shelf as Shelf

type Html = String

renderPosts :: (HasField "posts" row [PostRow]) => row -> Html
renderPosts row = show (length row.posts)

renderedPosts :: [Html]
renderedPosts = map renderPosts postsOnly ++ map renderPosts postsAndDrafts

skippedAuthor :: AuthorWith Skip Skip
skippedAuthor = AuthorWith {author = AuthorRow, posts = skipped, drafts = skipped}

postsOnly :: [AuthorWith (Load ()) Skip]
postsOnly =
  Author.findMany
    AuthorQuery
      { include_ = AuthorInclude {posts = load, drafts = skip},
        select_ = ()
      }

postsAndDrafts :: [AuthorWith (Load ()) (Load ())]
postsAndDrafts =
  Author.findMany
    AuthorQuery
      { include_ = AuthorInclude {posts = load, drafts = load},
        select_ = ()
      }

nestedShelves :: [ShelfWith (Load (BookInclude (Load ()))) Skip]
nestedShelves =
  Shelf.findMany
    ShelfQuery
      { include_ =
          ShelfInclude
            { books = loadWith BookInclude {chapters = load},
              tags = skip
            },
        select_ = ()
      }

requiredAuthor :: PostWith (Load ()) Skip -> AuthorRow
requiredAuthor row = row.author

nullableReviewer :: PostWith Skip (Load ()) -> Maybe AuthorRow
nullableReviewer row = row.reviewer

loadedChapters :: ShelfWith (Load (BookInclude (Load ()))) Skip -> [ChapterRow]
loadedChapters row =
  case row.books of
    book : _ -> book.chapters
    [] -> []

_useReviewerQuery :: [PostWith (Load ()) (Load ())]
_useReviewerQuery =
  findManyPosts
    PostQuery
      { include_ = PostInclude {author = load, reviewer = load},
        select_ = ()
      }
