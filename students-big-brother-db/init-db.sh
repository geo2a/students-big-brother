#!/bin/bash
set -e

# Create students_big_brother_db database and students_big_brother with
# password supplied with SBB_DB_PWD environmental variable
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    CREATE USER students_big_brother WITH PASSWORD '$SBB_DB_PWD';
    CREATE DATABASE students_big_brother_db OWNER students_big_brother;
    GRANT ALL PRIVILEGES ON DATABASE students_big_brother_db
                                  TO students_big_brother;
EOSQL

# Create necessary table in students_big_brother_db
psql -v ON_ERROR_STOP=1 -U students_big_brother -d students_big_brother_db <<-EOSQL
    CREATE TYPE user_role AS ENUM ('admin', 'teacher');

    CREATE TABLE users ( user_id serial PRIMARY KEY
                       , username varchar(32) NOT NULL
                       , password varchar(32) NOT NULL -- security? Ha-Ha.
                       , role user_role NOT NULL
                       );

    CREATE TABLE files ( file_id serial PRIMARY KEY
                       , file_path varchar(32) NOT NULL
                       , file_contents varchar(65536)
                       , modification_time timestamptz NOT NULL
                       , student_id integer NOT NULL
                       );


    CREATE TABLE students ( student_id integer PRIMARY KEY
                          , first_name varchar(32) NOT NULL
                          , middle_name varchar(32) NOT NULL
                          , last_name varchar(32) NOT NULL
                          -- It would be great to check if user with 
                          -- this id actually has role 'teacher'  
                          , teacher_id integer REFERENCES users (user_id) NOT NULL
                          );

    GRANT USAGE, SELECT ON SEQUENCE files_file_id_seq TO students_big_brother;
EOSQL
