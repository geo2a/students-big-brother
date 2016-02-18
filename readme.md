# TUTORIAL IS UNDER CONSTRUCTION

# Students Big Brother

Some logo here 

## Description 

A web service to help teacher in battle with students shyness in programming classroom.

## Developer's tutorial

The system consists of two main parts: web server and client daemons 

### Running a server

You are supposed to have haskell [stack](http://docs.haskellstack.org/en/stable/README.html) installed 

First of all you should start docker container with the database

TODO: more info about docker container, e.g. pulling the image, etc.

```
docker start postgres-students-big-brother
```

Then you should run the server: 

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

You also should host static front-end files with some web-server, for example nodejs http-server: 

```
nodejs ~/node_modules/http-server/bin/http-server frontend/
```