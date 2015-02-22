BEGIN;

CREATE TABLE account (
  id     SERIAL       NOT NULL PRIMARY KEY
, type   VARCHAR(100) NOT NULL
, number INTEGER      NOT NULL
, name   VARCHAR(100) NOT NULL
);

CREATE TABLE place_category(
  id   SERIAL      NOT NULL PRIMARY KEY
, name VARCHAR(25) NOT NULL
);

CREATE TABLE place (
  id                SERIAL       NOT NULL PRIMARY KEY
, name              VARCHAR(100) NOT NULL UNIQUE
, place_category_id INTEGER               REFERENCES place_category(id)
);

CREATE TABLE transaction (
  id       SERIAL           NOT NULL PRIMARY KEY
, date     DATE             NOT NULL
, amount   DOUBLE PRECISION NOT NULL -- These should be numerics, but opaleye doesn't support them well.
, balance  DOUBLE PRECISION NOT NULL -- Floating point numbers and money are bad, mmkay?
, type     VARCHAR(30)      NOT NULL CHECK (type in
  ('visa','eftpos','foreign_currency_conversion_fee','atm_operator_fee'
  ,'atm_withdrawal','direct_credit','internet_transfer_credit'
  ,'internet_transfer_debit'))
, place_id INTEGER                REFERENCES place(id)
);

CREATE TABLE transaction_visa (
  transaction_id INTEGER     NOT NULL REFERENCES transaction(id)
, purchase_date  DATE        NOT NULL
, currency_code  VARCHAR(10) NOT NULL
, country_code   VARCHAR(10) NOT NULL
);

CREATE TABLE transaction_atm_operator_fee (
  transaction_id       INTEGER     NOT NULL REFERENCES transaction(id)
, atm_transaction_type VARCHAR(10) NOT NULL
);

CREATE TABLE transaction_direct_credit (
  transaction_id       INTEGER     NOT NULL REFERENCES transaction(id)
, bsb                  INTEGER     NOT NULL
);

CREATE TABLE transaction_internet_transfer (
  transaction_id       INTEGER     NOT NULL REFERENCES transaction(id)
, account              INTEGER     NOT NULL
, ref                  VARCHAR(25) NOT NULL
);

COMMIT;
