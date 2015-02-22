BEGIN;

INSERT INTO place_category (name) VALUES ('Shirts');
INSERT INTO place (name,place_category_id) VALUES ('TeeTurtle',1);

COMMIT;
