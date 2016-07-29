import Ember from 'ember';

export default Ember.Route.extend({
  model(params) {
    const username = "AlanTuring"
    const password = "123"
    const hostname = "localhost"
    const fetchOptions = { method: "GET"
                         , headers: { 'Accept': 'application/json'
                                    , "Authorization": "Basic " +
                                       btoa(username + ":" + password)
                                    }
                         , mode: "cors"
                         }
    const errorHandler = error => {
      console.log(error)
    }
    return fetch("http://" + hostname + ":8083/files", fetchOptions)
                .then(response => response.json())
                .catch(errorHandler)
    // return  Ember.$.getJSON('/student-source-files');
  }
});
