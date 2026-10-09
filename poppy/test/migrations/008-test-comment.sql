CREATE TABLE IF NOT EXISTS test_comment (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  parent_id UUID REFERENCES test_comment (id),
  body TEXT NOT NULL
);
