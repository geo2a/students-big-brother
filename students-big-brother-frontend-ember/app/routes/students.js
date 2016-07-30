import Ember from 'ember';
import  _ from 'lodash/lodash';
import cfg from '../config/environment';

export default Ember.Route.extend({
  queryParams: {
    sid: {
      refreshModel: true
    }
  },
  model(params) {
    const fetchOptions = { method: "GET"
                         , headers: { 'Accept': 'application/json'
                                    , "Authorization": "Basic " +
                                       btoa(cfg.APP.SBB_USER_NAME + ":" +
                                            cfg.APP.SBB_USER_PASSWORD)
                                    }
                         , mode: "cors"
                         }
    const errorHandler = error => {
      console.log(error)
    }
    return Ember.RSVP.hash({
      // currentStudent: Ember.$.getJSON('/student-source-files?sid=' + params.student_id),
      allStudents: fetch("http://" + cfg.SBB_HOST + ":" + cfg.SBB_PORT +
                         "/files", fetchOptions)
                        .then(response => response.json())
                        .catch(errorHandler)
    });
  }
});
