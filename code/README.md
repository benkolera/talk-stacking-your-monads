Setup
=====

Installation
------------

First you'll want to create a sandbox to install all of the deps into:

cabal sandbox init
cabal install --only-dependencies

To run the code and tests, you'll need a locally running postgres
server that will accept a passwordless login from the current user.

Tests
-----
The test cases create a database with a random name for each test,
so the current user will need to have permission to create a database
(having superuser priviledges is the easiest way to achieve this).

If you need another way to login to the DB (say with a password) you'll
have to change the connectPostgreSQL string in the test case, sadly.

Run the tests by running:
cabal test

Code
----
To run the code, you'll need to create a database with the desired
schema in it.

createdb transaction_importer
psql transaction_importer < schema.sql

Then fill in app.cfg with a user,database (and an optional port,password and host)

cabal run -- tests/csv/ok.csv
