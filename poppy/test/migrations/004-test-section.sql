CREATE TABLE IF NOT EXISTS test_section (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  chapter_id UUID NOT NULL REFERENCES test_chapter (id) ON DELETE CASCADE,
  label TEXT NOT NULL
);
