# TUTORIAL IS UNDER CONSTRUCTION

# Students Big Brother

Some logo here 

## Description 

A web service to help teacher in battle with students shyness in programming classroom.

## Developer's tutorial

The system consists of two main parts: web server and client daemons 

### DB initialization

Let's assume you have clear installation of postgresql.

First of all, login as postgre user

```
sudo -u postgres psql postgres
```

And create user for application

```
CREATE USER students_big_brother WITH SUPERUSER PASSWORD 'pwd';
```

Then create a database

```
CREATE DATABASE students_big_brother_db OWNER students_big_brother;
```

And now you are able to connect as a newly created user to created newly 
database and initialise db schema. It can be easily done by this bash command: 

```
$ psql -U students_big_brother -d students_big_brother_db -a -f db_init.sql
``` 

### Running a server

You are supposed to have haskell [stack](http://docs.haskellstack.org/en/stable/README.html) installed 

Run the server: 

```
stack exec students-big-brother-exe -- --server <path_to_server_configuration>
```

Where path_to_server_configuration if like this: "aux/server-cfg.json"

### Running the client daemons

```
stack exec students-big-brother-exe -- --client <path_to_client_configuration>
```

Where path_to_client_configuration if like this: "aux/client-daemon-cfg-1.json"

You can run multiple instances of client daemons with different IDs.

### Running nodejs http-server to serve static files

You also should host static front-end files with some web-server, for example nodejs http-server: 

```
nodejs ~/node_modules/http-server/bin/http-server frontend/
```