import { moduleForModel, test } from 'ember-qunit';

moduleForModel('student-source-file', 'Unit | Model | student source file', {
  // Specify the other units that are required for this test.
  needs: []
});

test('it exists', function(assert) {
  let model = this.subject();
  // let store = this.store();
  assert.ok(!!model);
});
