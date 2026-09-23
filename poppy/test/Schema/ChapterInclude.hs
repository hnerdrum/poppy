{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.ChapterInclude
  ( ChapterInclude (..),
    NoInclude (..),
    ResolveInclude,
    unwrapNoInclude,
    ChapterWithSections (..)
  )
where

import Data.Kind (Type)
import Poppy.Db (Db)
import Poppy.Include
  ( ExecuteInclude (..),
    IncludesJoin (..),
    NestInclude (..)
  )
import Poppy.Operations (findMany)
import Poppy.SelectIn
  ( findByIn,
    prepareIncludeRootQuery,
    emptyGroups,
    indexHasMany,
    lookupGroups
  )
import Schema.Chapter (ChapterTable, ChapterRow (..))
import Schema.Section (SectionTable, SectionRow (..))
import qualified Schema.Section as Section

newtype ChapterInclude = ChapterInclude
  { sections :: Bool
  }
  deriving (Show, Eq)

data ChapterWithSections = ChapterWithSections
  { chapter :: ChapterRow,
    sections :: [SectionRow]
  }
  deriving (Show, Eq)

type family ResolveInclude preset :: Type
type instance ResolveInclude NoInclude = ChapterRow
type instance ResolveInclude ChapterInclude = ChapterWithSections

newtype NoInclude = NoInclude ChapterInclude
  deriving (Show, Eq)

unwrapNoInclude :: NoInclude -> ChapterInclude
unwrapNoInclude (NoInclude include) = include

instance IncludesJoin ChapterInclude where
  includesJoin include = include.sections

instance NestInclude ChapterInclude ChapterWithSections where
  type RootRow ChapterInclude = ChapterRow
  wrapRoot _ chapter = ChapterWithSections {chapter, sections = []}

loadChapterInclude :: ChapterInclude -> [ChapterRow] -> Db [ChapterWithSections]
loadChapterInclude include roots = do
  sectionsMap <-
    if include.sections
      then indexHasMany (.chapterRef) <$> findByIn @SectionTable @SectionRow Section.sectionChapterRef (map (.id) roots)
      else pure emptyGroups
  pure
    [
      ChapterWithSections {
        chapter = root,
        sections = lookupGroups root.id sectionsMap
      }
    | root <- roots
    ]

instance {-# OVERLAPPING #-} ExecuteInclude ChapterTable ChapterInclude ChapterWithSections where
  executeInclude include modifier = do
    roots <- findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable modifier)
    loadChapterInclude include roots

