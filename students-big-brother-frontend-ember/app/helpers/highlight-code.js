import Ember from 'ember';
import Highlight from 'highlight.js';

export function highlightCode([code, ...rest]) {
  Highlight.configure({
    languages: ["cpp"]
  });
  // console.log(Highlight.highlightAuto(code));
  return Highlight.highlightAuto(code).value;
  // return "code";
}

export default Ember.Helper.helper(highlightCode);
