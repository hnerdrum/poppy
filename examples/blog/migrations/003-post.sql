CREATE TABLE IF NOT EXISTS post (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  author_id UUID NOT NULL REFERENCES author (id),
  title TEXT NOT NULL,
  status articlestatus NOT NULL
);
