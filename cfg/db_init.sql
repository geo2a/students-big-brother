----------------------------------------------------
-- Students Big Brother postgresql initialization --
---------------------------------------------------- 

-- Please, see readme.md for more complete and up-to-date info

-- First two lines have to be executed under postgresql user 

-- CREATE USER students_big_brother WITH SUPERUSER PASSWORD '123';

-- CREATE DATABASE students_big_brother_db OWNER students_big_brother;

-- After that you can execute this script to initialize table like this: 
-- psql -U students_big_brother -d students_big_brother_db -a -f db_init.sql

CREATE TABLE files ( id serial CONSTRAINT firstkey PRIMARY KEY
                   , clientID integer NOT NULL
                   , filePath varchar(32) NOT NULL
                   , fileContents varchar(4096)
                   );

GRANT USAGE, SELECT ON SEQUENCE files_id_seq TO students_big_brother_db;

ALTER SEQUENCE files_id_seq RESTART WITH 1;

-- insert into files(clientID, filePath, fileContents) values (1, 'main.hs', 
--                                                             'lalalalalala');