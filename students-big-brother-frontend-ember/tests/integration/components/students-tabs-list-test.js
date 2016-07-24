import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('students-tabs-list', 'Integration | Component | students tabs list', {
  integration: true
});

test('it renders', function(assert) {
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });

  this.render(hbs`{{students-tabs-list}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:
  this.render(hbs`
    {{#students-tabs-list}}
      template block text
    {{/students-tabs-list}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
