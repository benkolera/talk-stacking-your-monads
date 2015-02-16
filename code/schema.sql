BEGIN;

CREATE TABLE category(
  id   SERIAL      NOT NULL PRIMARY KEY
, name VARCHAR(25) NOT NULL
);

CREATE TABLE place ( 
  id       SERIAL       NOT NULL PRIMARY KEY
, place    VARCHAR(100) NOT NULL UNIQUE
, category INTEGER               REFERENCES category(id) 
);

CREATE TABLE transaction (
  id       SERIAL        NOT NULL PRIMARY KEY
, date     DATE          NOT NULL
, amount   NUMERIC(10,2) NOT NULL
, balance  NUMERIC(15,2) NOT NULL
);

COMMIT;
