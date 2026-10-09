{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Schema.Include.Chapter
  ( ChapterInclude (..)
  , ChapterWith (..)
  , ChapterWithPicked (..)
  , ChapterSections
  , ChapterResult
  , ChapterRead
  , LoadChapter (..)
  , toChapterWithPicked
  )
where

import Poppy.Internal.Generated
  ( Db,
    IncludeFor,
    Load (..),
    Skip (..),
    Skipped,
    ValidEdge,
    skipped,
    OmitSelect (..),
    findByIn,
    indexHasMany,
    lookupGroups
  )
import Schema.Chapter (ChapterPicked, ChapterRow (..), ChapterSelect, toChapterPicked)
import qualified Schema.Section as Section
import Schema.Section (SectionRow (..), SectionTable)

data ChapterInclude sections = ChapterInclude
  { sections :: sections
  }
  deriving (Show, Eq)

data ChapterWith sections = ChapterWith
  { chapter :: ChapterRow,
    sections :: ChapterSections sections
  }

deriving instance (Eq ChapterRow, Eq (ChapterSections sections)) => Eq (ChapterWith sections)
deriving instance (Show ChapterRow, Show (ChapterSections sections)) => Show (ChapterWith sections)

data ChapterWithPicked sections = ChapterWithPicked
  { chapter :: ChapterPicked,
    sections :: ChapterSections sections
  }

deriving instance (Eq ChapterPicked, Eq (ChapterSections sections)) => Eq (ChapterWithPicked sections)
deriving instance (Show ChapterPicked, Show (ChapterSections sections)) => Show (ChapterWithPicked sections)

toChapterWithPicked :: ChapterSelect -> ChapterWith sections -> ChapterWithPicked sections
toChapterWithPicked select_ nested =
  ChapterWithPicked
    { chapter = toChapterPicked select_ nested.chapter,
      sections = nested.sections
    }

type family ChapterSections edge where
  ChapterSections Skip = Skipped "sections" [SectionRow]
  ChapterSections (Load SectionTable ()) = [SectionRow]

type family ChapterResult include where
  ChapterResult () = ChapterRow
  ChapterResult (ChapterInclude sections) = ChapterWith sections

type family ChapterRead include select where
  ChapterRead () OmitSelect = ChapterRow
  ChapterRead () ChapterSelect = ChapterPicked
  ChapterRead (ChapterInclude sections) OmitSelect = ChapterWith sections
  ChapterRead (ChapterInclude sections) ChapterSelect = ChapterWithPicked sections

class LoadChapterSections edge where
  loadChapterSections :: edge -> [ChapterRow] -> Db [ChapterSections edge]

instance LoadChapterSections Skip where
  loadChapterSections Skip roots = pure (map (const skipped) roots)

instance LoadChapterSections (Load SectionTable ()) where
  loadChapterSections edge roots = do
    rows <- findByIn @SectionTable @SectionRow Section.sectionChapterRef (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    let grouped = indexHasMany (.chapterRef) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Section" edge) => LoadChapterSections edge where
  loadChapterSections _ roots = pure (map (const skipped) roots)

class LoadChapter sections where
  loadChapter :: ChapterInclude sections -> [ChapterRow] -> Db [ChapterWith sections]

instance (LoadChapterSections sections, ValidEdge "Section" sections) => LoadChapter sections where
  loadChapter include roots = do
    sectionsLoaded <- loadChapterSections include.sections roots
    pure
      [ ChapterWith
          { chapter = root,
            sections = sectionsLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Section" sections) => IncludeFor "Chapter" (ChapterInclude sections)

