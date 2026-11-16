//Provides: greet_punctuation
//Requires: caml_string_of_jsstring
function greet_punctuation(_unit) {
  return caml_string_of_jsstring("!");
}
