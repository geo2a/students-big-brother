# Example API requests

* Obtain files via GET request

```
curl -u Alan:Turing localhost:8083/files
```

* Send current files list for user 42 via POST request

```
curl -X POST -d "[{\"path\": \"file1.hs\", \"contents\": \"source code 1\"}, {\"path\": \"file2.hs\", \"contents\": \"source code 2\"}]" -H 'Accept: application/json' -H 'Content-type: application/json' localhost:8083/files/42
```

* Register a new teacher

```
curl -X POST -d "{\"username\": \"AlanTuring\", \"password\": \"123\"}" -H 'Accept: application/json' -H 'Content-type: application/json' localhost:8083/admin/register-teacher
```

* Register a new student

```
curl -X POST -d "{\"s_id\": \"42\", \"f_name\": \"Alan\", \"m_name\": \"Alan\", \"l_name\": \"Turing\"}" -H 'Accept: application/json' -H 'Content-type: application/json' localhost:8083/register-student
```

* Delete a specific teacher
```
curl -X POST -d "21" -H 'Accept: application/json' -H 'Content-type: application/json' localhost:8083/admin/delete-teacher
```
