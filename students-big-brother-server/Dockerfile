FROM ubuntu:latest

RUN apt-get update && apt-get install -y libpq5

RUN mkdir -p /var/students-big-brother-server
ADD .stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/students-big-brother-server/students-big-brother-server /var/students-big-brother-server/
ADD sbb-config.json /var/students-big-brother-server/sbb-config.json

ENTRYPOINT ["/var/students-big-brother-server/students-big-brother-server", "/var/students-big-brother-server/sbb-config.json"]
