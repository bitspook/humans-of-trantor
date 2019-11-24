CREATE SCHEMA store;
SET search_path TO store, PUBLIC;

CREATE EXTENSION "uuid-ossp";

CREATE TABLE store (
  id UUID NOT NULL DEFAULT uuid_generate_v1mc(),
  name VARCHAR (200) NOT NULL,
  version VARCHAR (16),
  payload JSON,
  created_at TIMESTAMP DEFAULT NOW(),
  PRIMARY KEY (ID)
);
