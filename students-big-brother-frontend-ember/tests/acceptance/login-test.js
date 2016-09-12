import { test } from 'qunit';
import moduleForAcceptance from 'students-big-brother-frontend-ember/tests/helpers/module-for-acceptance';

moduleForAcceptance('Acceptance | login | General');

test('visiting /login', function(assert) {
  visit('/login');
  andThen(() => assert.equal(currentURL(), '/login'));
});

moduleForAcceptance('Acceptance | login | Not logged in');

test('Login form is present if not logged in', function(assert) {
  visit('/login');
  andThen(() => {
    let form = find('.log-in-form')[0];
    assert.notEqual(form, null);
  });
});

test('No user token in local storage if not logged in', function(assert) {
  visit('/login');
  andThen(() => assert.equal(localStorage.getItem("user"), null));
});

test('Can log in', function(assert) {
  visit('/login');
  fillIn("input[type='text']", '123');
  fillIn("input[type='password']", '123');
  click('.log-in-form__button');
  andThen(() => assert.equal( localStorage.getItem("user")
                            , "{username: 123, password: 123}"))
});

moduleForAcceptance('Acceptance | login | Logged in');
