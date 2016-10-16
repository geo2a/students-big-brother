/* jshint node: true */

module.exports = function(environment) {
  var ENV = {
    modulePrefix: 'students-big-brother-frontend-ember',
    environment: environment,
    baseURL: '/',
    locationType: 'auto',
    EmberENV: {
      FEATURES: {
        // Here you can enable experimental features on an ember canary build
        // e.g. 'with-controller': true
      }
    },

    APP: {
      // Here you can pass flags/options to your application instance
      // when it is created
    }
  };

  if (environment === 'development') {
    ENV.APP.SBB_USER_NAME = ""
    ENV.APP.SBB_USER_PASSWORD = ""
    ENV.APP.SBB_HOST = "54.213.202.245"
    ENV.APP.SBB_PORT = "8083"

    // ENV.APP.LOG_RESOLVER = true;
    // ENV.APP.LOG_ACTIVE_GENERATION = true;
    // ENV.APP.LOG_TRANSITIONS = true;
    // ENV.APP.LOG_TRANSITIONS_INTERNAL = true;
    // ENV.APP.LOG_VIEW_LOOKUPS = true;
  }

  if (environment === 'test') {
    // Testem prefers this...
    ENV.baseURL = '/';
    ENV.locationType = 'none';

    // keep test console output quieter
    ENV.APP.LOG_ACTIVE_GENERATION = false;
    ENV.APP.LOG_VIEW_LOOKUPS = false;

    ENV.APP.rootElement = '#ember-testing';
  }

  if (environment === 'production') {
    ENV.APP.SBB_USER_NAME = ""
    ENV.APP.SBB_USER_PASSWORD = ""
    ENV.APP.SBB_HOST = "54.213.202.245"
    ENV.APP.SBB_PORT = "8083"
  }


  return ENV;
};
