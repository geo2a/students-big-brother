import Ember from 'ember';

export default Ember.Route.extend({
  model(params) {
    return  Ember.$.getJSON('/student-source-files');
  }
});