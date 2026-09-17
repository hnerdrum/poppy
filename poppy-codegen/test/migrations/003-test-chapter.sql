CREATE TABLE IF NOT EXISTS test_chapter (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  book_id UUID NOT NULL REFERENCES test_book (id) ON DELETE CASCADE,
  heading TEXT NOT NULL
);
