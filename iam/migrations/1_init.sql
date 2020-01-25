CREATE SCHEMA iam;
-- enabled extensions

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- functions

CREATE OR REPLACE FUNCTION trigger_set_timestamp()
  RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

--- Setting the search path before creating extensions restrict extensions to be created in search path
SET search_path TO iam, PUBLIC;

-- identity is a how a client who can create a session is identified
CREATE TABLE identity (
  id UUID NOT NULL DEFAULT uuid_generate_v1mc(),
  email TEXT NOT NULL UNIQUE,
  password TEXT NOT NULL,

  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  PRIMARY KEY (id)
);

CREATE TRIGGER set_identity_timestamp
  BEFORE UPDATE ON identity
  FOR EACH ROW
    EXECUTE PROCEDURE trigger_set_timestamp();

-- Session is an active user session
CREATE TABLE session (
  id UUID NOT NULL DEFAULT uuid_generate_v1mc(),
  identity_id UUID NOT NULL REFERENCES identity(id),

  created_at TIMESTAMP DEFAULT NOW(),
  last_used_at TIMESTAMP DEFAULT NOW(),
  PRIMARY KEY (id)
);

CREATE TRIGGER set_session_timestamp
  BEFORE UPDATE ON session
  FOR EACH ROW
    EXECUTE PROCEDURE trigger_set_timestamp();
