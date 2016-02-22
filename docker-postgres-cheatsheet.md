# Docker + Postgresql cheatsheet 

* Connect to docker container

```
$ docker exec -ti postgres-students-big-brother "/bin/bash"
```

* Became a postgres user

```
# su - postgres
```

* Connect to db

```
# psql --dbname=students_big_brother_db
```

* Delete everything from table

```
psql> delete from files;
```