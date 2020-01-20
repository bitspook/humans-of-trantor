CREATE SCHEMA iam;
SET search_path TO iam, PUBLIC;

CREATE EXTENSION "uuid-ossp";

CREATE OR REPLACE FUNCTION trigger_set_timestamp()
  RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE identity (
  id UUID NOT NULL DEFAULT uuid_generate_v1mc(),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  PRIMARY KEY (ID)
);

CREATE TRIGGER set_identity_timestamp
  BEFORE UPDATE ON identity
  FOR EACH ROW
    EXECUTE PROCEDURE trigger_set_timestamp();
