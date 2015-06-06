BEGIN;

INSERT INTO account (type,number,name) VALUES ('Savings',123456,'Savings Account');
INSERT INTO place_category (name) VALUES ('Shirts');
INSERT INTO place (name,place_category_id) VALUES ('TeeTurtle',1);
INSERT INTO transaction (date,amount,balance,type,place_id,account_id) VALUES (
  date(now())
, -10
, 90
, 'internet_transfer_credit'
, 1
, 1
);

COMMIT;
