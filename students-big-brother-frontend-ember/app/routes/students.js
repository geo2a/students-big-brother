import Ember from 'ember';
import  _ from 'lodash/lodash';

export default Ember.Route.extend({
  queryParams: {
    sid: {
      refreshModel: true
    }
  },
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

    return Ember.RSVP.hash({
      // currentStudent: Ember.$.getJSON('/student-source-files?sid=' + params.student_id),
      allStudents: fetch("http://" + hostname + ":8083/files", fetchOptions)
                        .then(response => response.json())
                        .catch(errorHandler)
    });
  }
});
