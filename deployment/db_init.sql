----------------------------------------------------
-- Students Big Brother postgresql initialization --
----------------------------------------------------

-- Please, see readme.md for more complete and up-to-date info

-- Next two lines have to be executed under postgresql user

-- CREATE USER students_big_brother WITH SUPERUSER PASSWORD '123';

-- CREATE DATABASE students_big_brother_db OWNER students_big_brother;

-- After that you can execute this script to initialize table like this:
-- psql -U students_big_brother -d students_big_brother_db -a -f db_init.sql

-- CREATE TABLE students ( student_id serial PRIMARY KEY
--                       , first_name varchar(32)
--                       , last_name  varchar(32)
--                       );

CREATE TABLE files ( file_id serial PRIMARY KEY
                   , file_path varchar(32) NOT NULL
                   , file_contents varchar(4096)
                   , student_id integer NOT NULL
                   );

CREATE TABLE teachers ( teacher_id serial PRIMARY KEY
                      , username varchar(32) NOT NULL
                      , password varchar(32) NOT NULL -- security? Ha-Ha.
                      );

CREATE TABLE students ( student_id integer PRIMARY KEY
                      , first_name varchar(32) NOT NULL
                      , middle_name varchar(32) NOT NULL
                      , last_name varchar(32) NOT NULL
                      );

GRANT USAGE, SELECT ON SEQUENCE files_file_id_seq TO students_big_brother;
GRANT USAGE, SELECT ON SEQUENCE students_student_id_seq TO students_big_brother;
ALTER SEQUENCE files_file_id_seq RESTART WITH 1;
ALTER SEQUENCE students_student_id_seq RESTART WITH 1;

--------------------------------
-- Some test data and queries --
--------------------------------

-- Insert new student
-- INSERT INTO students (student_id, first_name, last_name) VALUES
--                      (1, 'John', 'Lennon')
--        RETURNING student_id;

-- Insert new file
-- INSERT INTO files (file_id, file_path, file_contents, student_id) VALUES
--                      (DEFAULT, 'song1', 'Let it be', 1);

-- SELECT students.student_id, file_id, file_path, file_contents FROM
--   students INNER JOIN files ON students.student_id = files.student_id;

-- Insert new teacher
-- INSERT INTO teachers (teacher_id, username, password)
--   VALUES (DEFAULT,'lal','lal') RETURNING teacher_id;
