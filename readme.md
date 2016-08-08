# Students Big Brother

Some logo here

[![Build Status](https://travis-ci.org/geo2a/sbb-travis-test.svg?branch=master)](https://travis-ci.org/geo2a/sbb-travis-test)

## Description

A web service to help teacher in battle with students shyness in programming classroom.

## Developer's tutorial

The system consists of three parts: web server, client daemons and frontend server (teachers Web-interface)

### DB initialization

Let's assume you have clear installation of postgresql.

First of all, login as postgres user

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
$ psql -U students_big_brother -d students_big_brother_db -h localhost -a -f db_init.sql
```

### Running a server

You are supposed to have haskell [stack](http://docs.haskellstack.org/en/stable/README.html) installed

Run the server:

```
stack exec students-big-brother-server -- <path_to_server_configuration.json>
```

You can find example config in students-big-brother-server/example-server-cfg.json


### Running the client daemons

```
stack exec students-big-brother-student -- <path_to_client_configuration.json>
```

You can find example config in students-big-brother-client/example-client-daemon-cfg.json

You can run multiple instances of client daemons with different IDs.

### Running [Ember.js](http://emberjs.com/) development http-server

```
cd students-big-brother-frontend-ember
ember s
```
