CREATE TABLE IF NOT EXISTS test_shelf (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS test_book (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  shelf_id UUID NOT NULL REFERENCES test_shelf (id),
  title TEXT NOT NULL
);
