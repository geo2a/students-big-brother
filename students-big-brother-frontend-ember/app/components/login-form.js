import Ember from 'ember';

import ENV from 'students-big-brother-frontend-ember/config/environment';

export default Ember.Component.extend({
  actions:{
    authenticate() {
      const user = this.getProperties('username', 'password');
      const credentials = btoa(user.username + ":" + user.password);
      const fetchOptions =
        { method: "GET"
        , headers: { 'Accept': 'application/json'
                   , "Authorization":
                       `Basic ${credentials}`
                   }
        , mode: "cors"
        };
      const successHandler = response => {
        if (response.ok) {
          localStorage.setItem('user',
            JSON.stringify(credentials)
          );
          document.location = "/";
        }
      }
      return fetch("http://" + ENV.APP.SBB_HOST + ":" + ENV.APP.SBB_PORT +
                           "/files", fetchOptions)
                           .then(handleErrors)
                           .then(successHandler)
                           .catch(error => console.log(error) );

    }
  }
});

function handleErrors(response) {
    if (!response.ok) {
        switch (response.status) {
          case 403:
            console.log("Forbidden");
            break;
          case 404:
            console.log("Not found");
            break;
          case 401:
            console.log("Anauthtorized");
            break;
          default:
            console.log(response.statusText);
        }
        document.location = "/login";
    }
    return response;
}
