import Ember from 'ember';

export default Ember.Route.extend({
  queryParams: {
    sid: {
      refreshModel: true
    }
  },
  model(params) {
    return Ember.RSVP.hash({
      currentStudent: this.get('store')
        .query('studentSourceFile', { sid: params.student_id }),
      allStudents: this.get('store').findAll('studentSourceFile')
    });
  }
});
