CREATE TYPE poststatus AS ENUM ('draft', 'published');

CREATE TABLE IF NOT EXISTS test_author (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS test_post (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  author_id UUID NOT NULL REFERENCES test_author (id),
  title TEXT NOT NULL,
  status poststatus NOT NULL
);
