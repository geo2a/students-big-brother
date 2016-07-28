import Ember from 'ember';
import  _ from 'lodash/lodash';

export default Ember.Route.extend({
  queryParams: {
    sid: {
      refreshModel: true
    }
  },
  model(params) {
    return Ember.RSVP.hash({
      currentStudent: Ember.$.getJSON('/student-source-files?sid=' + params.student_id),
      allStudents: Ember.$.getJSON('/student-source-files')
    });
  }
});
