CREATE TABLE IF NOT EXISTS test_packet (
  id UUID NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  amount NUMERIC NOT NULL,
  payload JSONB NOT NULL
);
