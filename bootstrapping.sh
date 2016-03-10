#!/bin/bash

# Running nodejs http-server to host frontend static files

nodejs ~/node_modules/http-server/bin/http-server frontend/

# Running server 

stack exec students-big-brother-exe -- --server aux/server-cfg.json

# Running client daemons  

stack exec students-big-brother-exe -- --client aux/client-daemon-cfg-1.json

stack exec students-big-brother-exe -- --client aux/client-daemon-cfg-2.json

stack exec students-big-brother-exe -- --client aux/client-daemon-cfg-3.json