"use strict"

const username = "Alan"
const password = "Turing"

const fetchOptions = { method: "GET"
                     , headers: { 'Accept': 'application/json'
                                , "Authorization": "Basic " +
                                   btoa(username + ":" + password)
                       }
                     , mode: "cors"
                     }

async function main() {
  const t = await fetch("http://localhost:8083/files", fetchOptions)
                  .then(response => response.json())
                  .then(data => console.log(JSON.stringify(data)))
                  .catch(error => console.log(error))
  console.log(t)
}
