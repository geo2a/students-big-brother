import Ember from 'ember';
import  _ from 'lodash/lodash';
import cfg from 'students-big-brother-frontend-ember/config/environment';

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
      currentStudent:
        fetch("http://" + cfg.APP.SBB_HOST + ":" + cfg.APP.SBB_PORT +
              "/files?s_id=" + params.student_id, fetchOptions)
             .then(response => response.json())
             .catch(errorHandler),
      allStudents: fetch("http://" + cfg.APP.SBB_HOST + ":" + cfg.APP.SBB_PORT +
                         "/files", fetchOptions)
                        .then(response => response.json())
                        .then(data => _.uniq(data, 'student.s_id'))
                        .catch(errorHandler)
    });
  }
});
