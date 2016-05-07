# Example API requests

* Obtain files for all users as JSON via GET request

```
curl -H 'Accept: application/json' localhost:8083/files
```

* Obtain files for all users as HTML via GET request

```
curl -H 'Accept: text/html' localhost:8083/files
```

* Send current files list for user 42 via POST request

```
curl -X POST -d "[{\"path\": \"file1.hs\", \"contents\": \"source code 1\"}, {\"path\": \"file2.hs\", \"contents\": \"source code 2\"}]" -H 'Accept: application/json' -H 'Content-type: application/json' localhost:8083/files/42
```

* Register a new teacher

```
curl -X POST -d "{\"username\": \"AlanTuring123\", \"password\": \"123\"}" -H 'Accept: application/json' -H 'Content-type: application/json' localhost:8083/admin/register-teacher
```
