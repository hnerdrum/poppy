module ORM.ShelfFixtures
  ( insertShelf,
    insertBook,
    insertChapter,
    insertSection,
    insertTag,
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import ORM (runDb)
import ORM.Db (DbPool)
import qualified ORM.Insert as Insert
import Schema.Book (BookCreate (..), BookRow (..), BookTable)
import Schema.Chapter (ChapterCreate (..), ChapterRow (..), ChapterTable)
import Schema.Section (SectionCreate (..), SectionRow (..), SectionTable)
import Schema.Shelf (ShelfCreate (..), ShelfRow (..), ShelfTable)
import Schema.Tag (TagCreate (..), TagRow (..), TagTable)
import Support.Assert (assertRight)

insertShelf :: DbPool -> Text -> IO ShelfRow
insertShelf pool name =
  runDb pool (Insert.insert @ShelfTable @ShelfRow (ShelfCreate {id = Nothing, name}))
    >>= assertRight

insertBook :: DbPool -> UUID -> Text -> IO BookRow
insertBook pool shelfId title =
  runDb pool (Insert.insert @BookTable @BookRow (BookCreate {id = Nothing, shelfId, title}))
    >>= assertRight

insertChapter :: DbPool -> UUID -> Text -> IO ChapterRow
insertChapter pool bookRef heading =
  runDb pool (Insert.insert @ChapterTable @ChapterRow (ChapterCreate {id = Nothing, bookRef, heading}))
    >>= assertRight

insertSection :: DbPool -> UUID -> Text -> IO SectionRow
insertSection pool chapterRef label =
  runDb pool (Insert.insert @SectionTable @SectionRow (SectionCreate {id = Nothing, chapterRef, label}))
    >>= assertRight

insertTag :: DbPool -> UUID -> Text -> IO TagRow
insertTag pool shelfId label =
  runDb pool (Insert.insert @TagTable @TagRow (TagCreate {id = Nothing, shelfId, label}))
    >>= assertRight
