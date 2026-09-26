module Poppy.AuthorFixtures
  ( insertAuthor,
    insertPost,
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy (runDb)
import Poppy.Db (DbPool)
import qualified Poppy.Insert as Insert
import Schema.Author (AuthorCreate (..), AuthorRow (..), AuthorTable)
import Schema.Post (PostCreate (..), PostRow (..), PostTable)
import Schema.PostStatus (PostStatus)
import Support.Assert (assertRight)

insertAuthor :: DbPool -> Text -> IO AuthorRow
insertAuthor pool name =
  runDb pool (Insert.insert @AuthorTable @AuthorRow (AuthorCreate {id = Nothing, name}))
    >>= assertRight

insertPost :: DbPool -> UUID -> Text -> PostStatus -> IO PostRow
insertPost pool authorId title status =
  runDb pool (Insert.insert @PostTable @PostRow (PostCreate {id = Nothing, authorId, title, status}))
    >>= assertRight
