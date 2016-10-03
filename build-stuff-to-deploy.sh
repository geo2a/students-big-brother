#!/bin/bash

DIRECTORY="deploy"
if [ ! -d "$DIRECTORY" ]; then
  mkdir $DIRECTORY
  echo "Directory" $DIRECTORY "created"
fi
echo "Directory" $DIRECTORY "already exists (it's ok though)"
echo "Building server executable"
cd ./students-big-brother-server/
stack build
cd ..
echo "Server executable built. Here it is: $DIRECTORY/students-big-brother-server"
cp ./students-big-brother-server/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/students-big-brother-server/students-big-brother-server deploy/

echo "Building frontend stuff"
cd ./students-big-brother-frontend-ember/
ember build --environment="production"
cd ..
cp -R ./students-big-brother-frontend-ember/dist/ deploy/
echo "Building frontend stuff built and put into $DIRECTORY/dist/"
