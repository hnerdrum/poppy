CREATE TABLE IF NOT EXISTS test_tag (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  shelf_id UUID NOT NULL REFERENCES test_shelf (id),
  label TEXT NOT NULL
);
