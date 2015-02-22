BEGIN;

INSERT INTO account (type,number,name) VALUES ('Savings',123456,'Savings Account');
INSERT INTO place_category (name) VALUES ('Shirts');
INSERT INTO place (name,place_category_id) VALUES ('TeeTurtle',1);

COMMIT;
