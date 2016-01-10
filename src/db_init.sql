CREATE TABLE files ( id serial CONSTRAINT firstkey PRIMARY KEY
                   , clientID integer NOT NULL
                   , filePath varchar(32) NOT NULL
                   , fileContents varchar(4096)
                   );

GRANT ALL ON ALL TABLES IN SCHEMA test TO root;

GRANT USAGE, SELECT ON SEQUENCE files_id_seq TO root;

ALTER SEQUENCE files_id_seq RESTART WITH 1;

insert into files(clientID, filePath, fileContents) values (1, 'main.hs', 'lalalalalala');
