import Ember from 'ember';
import  _ from 'lodash/lodash';
import ENV from 'students-big-brother-frontend-ember/config/environment';

export default Ember.Route.extend({
  queryParams: {
    sid: {
      refreshModel: true
    }
  },
  model(params) {
    const fetchOptions =
      { method: "GET"
      , headers: { 'Accept': 'application/json'
                 , "Authorization":
                     `Basic ${localStorage.getItem('user')}`
                 }
      , mode: "cors"
      };
    const errorHandler = error => {
      console.log(error)
    }
    return Ember.RSVP.hash({
      currentStudent:
        fetch("http://" + ENV.APP.SBB_HOST + ":" + ENV.APP.SBB_PORT +
              "/files?s_id=" + params.student_id, fetchOptions)
             .then(response => response.json())
             .catch(errorHandler),
      allStudents: fetch("http://" + ENV.APP.SBB_HOST + ":" + ENV.APP.SBB_PORT +
                         "/files", fetchOptions)
                        .then(handleErrors)
                        .then(response => response.json())
                        .then(data => _.uniq(data, 'student.student_id'))
                        .then(data => _.sortBy(data, 'student.last_name'))
                        .catch(errorHandler)
    });
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
