import Ember from 'ember';
import  _ from 'lodash/lodash';
import cfg from 'students-big-brother-frontend-ember/config/environment';

export default Ember.Route.extend({
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
    return fetch("http://" + cfg.APP.SBB_HOST + ":" + cfg.APP.SBB_PORT +
                         "/files", fetchOptions)
                        .then(response => response.json())
                        .then(data => _.uniq(data, 'student.s_id'))
                        .catch(errorHandler)
  }
});
