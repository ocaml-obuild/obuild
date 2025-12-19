open Test_framework
open Test_helpers

(** Comprehensive META parser error tests

    These tests systematically verify error handling for:
    - Predicate syntax errors
    - Field syntax errors
    - String literal errors
    - Package block errors
    - Semantic errors
*)

(** {1 Predicate Syntax Errors} *)

let test_unclosed_predicate_paren () =
  assert_meta_parse_error
    ~content:"archive(byte = \"foo.cma\""
    ~expected_msg:"expecting ')'"
    ~name:"unclosed predicate parenthesis"

let test_missing_predicate_close () =
  assert_meta_parse_error
    ~content:"archive(byte,native = \"foo.cma\""
    ~expected_msg:"expecting ')'"
    ~name:"missing close paren in predicate list"

let test_empty_predicate_parens () =
  (* This should actually work - empty predicates are valid *)
  assert_meta_parses
    ~content:"archive() = \"foo.cma\""
    ~name:"empty predicate parens"

let test_predicate_without_field () =
  assert_meta_parse_error
    ~content:"(byte) = \"foo.cma\""
    ~expected_msg:"unknown token"
    ~name:"predicate without field name"

let test_malformed_predicate () =
  assert_meta_parse_error
    ~content:"archive(byte,) = \"foo.cma\""
    ~expected_msg:"expecting ')'"
    ~name:"malformed predicate with trailing comma"

(** {1 Field Syntax Errors} *)

let test_missing_equals () =
  assert_meta_parse_error
    ~content:"version \"1.0.0\""
    ~expected_msg:"unknown token"
    ~name:"missing equals after field"

let test_missing_value () =
  assert_meta_parse_error
    ~content:"version ="
    ~expected_msg:"unknown token"
    ~name:"missing value after equals"

let test_field_without_value () =
  assert_meta_parse_error
    ~content:"version\narchive(byte) = \"foo.cma\""
    ~expected_msg:"unknown token"
    ~name:"field name without equals or value"

let test_duplicate_equals () =
  assert_meta_parse_error
    ~content:"version = = \"1.0.0\""
    ~expected_msg:"unknown token"
    ~name:"duplicate equals"

let test_invalid_field_chars () =
  (* Field names with special chars - lexer catches this *)
  assert_meta_parse_error
    ~content:"my-field! = \"value\""
    ~expected_msg:"meta lexing error"
    ~name:"field name with invalid characters"

(** {1 String Literal Errors} *)

let test_unclosed_string () =
  (* FIXED: Lexer now detects unclosed strings with position *)
  assert_meta_parse_error
    ~content:"version = \"1.0.0"
    ~expected_msg:"1.10: meta lexing error: unclosed string literal"
    ~name:"unclosed string literal with position"

let test_string_without_quotes () =
  (* Lexer error, not parser error *)
  assert_meta_parse_error
    ~content:"version = 1.0.0"
    ~expected_msg:"meta lexing error"
    ~name:"string value without quotes"

let test_empty_string () =
  (* Empty strings should be valid *)
  assert_meta_parses
    ~content:"version = \"\""
    ~name:"empty string value"

(** {1 Package Block Errors} *)

let test_unclosed_package_block () =
  (* FIXED: Unclosed package blocks now detected *)
  assert_meta_parse_error
    ~content:{|
version = "1.0"
package "sub" (
  version = "1.1"
|}
    ~expected_msg:"unclosed package block"
    ~name:"unclosed package block"

let test_package_without_name () =
  assert_meta_parse_error
    ~content:{|
package (
  version = "1.0"
)
|}
    ~expected_msg:"unknown token"
    ~name:"package without name"

let test_package_without_paren () =
  assert_meta_parse_error
    ~content:{|
package "sub"
  version = "1.0"
|}
    ~expected_msg:"unknown token"
    ~name:"package without opening paren"

let test_nested_package_error () =
  (* FIXED: Unclosed strings now detected even in nested packages *)
  assert_meta_parse_error
    ~content:{|
version = "1.0"
package "outer" (
  package "inner" (
    version = "1.1
  )
)
|}
    ~expected_msg:"unclosed string literal"
    ~name:"nested package with unclosed string"

(** {1 Append Operator Errors} *)

let test_append_without_value () =
  assert_meta_parse_error
    ~content:"archive(byte) += "
    ~expected_msg:"parsing archive failed"
    ~name:"append operator without value"

let test_append_to_nonexistent () =
  (* Appending to a field that wasn't defined - should this work? *)
  assert_meta_parses
    ~content:"archive(byte) += \"extra.cma\""
    ~name:"append to nonexistent field"

(** {1 Specific Field Errors} *)

let test_requires_invalid_syntax () =
  assert_meta_parse_error
    ~content:"requires(byte) == \"unix\""
    ~expected_msg:"parsing requires failed"
    ~name:"requires with double equals"

let test_archive_missing_predicate_close () =
  assert_meta_parse_error
    ~content:"archive(byte,native = \"foo.cma\""
    ~expected_msg:"expecting ')'"
    ~name:"archive missing predicate close"

let test_ppx_invalid_syntax () =
  assert_meta_parse_error
    ~content:"ppx(byte) \"./ppx.exe\""
    ~expected_msg:"parsing ppx failed"
    ~name:"ppx without equals"

let test_linkopts_invalid () =
  (* FIXED: linkopts now uses MetaParseError instead of failwith *)
  assert_exception_message
    ~test_func:(fun () -> parse_meta_string "linkopts(byte) \"invalid\"" "test")
    ~expected_substring:"parsing linkopts failed"
    ~name:"linkopts without equals"

(** {1 Complex Error Cases} *)

let test_multiple_errors () =
  (* Multiple errors - which one gets reported? *)
  assert_meta_parse_error
    ~content:{|
version = "1.0
archive(byte = "foo.cma
|}
    ~expected_msg:"unknown token"
    ~name:"multiple syntax errors"

let test_error_after_valid_content () =
  assert_meta_parse_error
    ~content:{|
version = "1.0"
description = "Valid package"
archive(byte "broken"
|}
    ~expected_msg:"expecting ')'"
    ~name:"error after valid content"

let test_invalid_escape_sequence () =
  (* Testing escape sequences in strings *)
  assert_meta_parses
    ~content:"description = \"Line 1\\nLine 2\\ttab\""
    ~name:"valid escape sequences"

(** {1 Lexer Error Cases} *)

let test_location_tracking () =
  (* Comprehensive test: Lexer errors include line.column position *)
  assert_meta_parse_error
    ~content:{|
version = "1.0"
requires = "unix"
$invalid_token
|}
    ~expected_msg:"4.0: meta lexing error: undefined character '$'"
    ~name:"location tracking in lexer errors"

let test_invalid_character () =
  assert_meta_parse_error
    ~content:"version = \"1.0\"\n@invalid"
    ~expected_msg:"2.0: meta lexing error: undefined character '@'"
    ~name:"invalid character with position"

let test_unexpected_eof () =
  assert_meta_parse_error
    ~content:"version = "
    ~expected_msg:"unknown token"
    ~name:"unexpected end of file"

(** {1 Edge Cases with Comments} *)

let test_comment_in_string () =
  (* Comments inside strings should be literal *)
  assert_meta_parses
    ~content:"description = \"Text with # not a comment\""
    ~name:"hash inside string is not comment"

let test_unclosed_after_comment () =
  (* FIXED: Unclosed strings now detected after comments with position *)
  assert_meta_parse_error
    ~content:{|
# This is a comment
version = "1.0
|}
    ~expected_msg:"3.10: meta lexing error: unclosed string literal"
    ~name:"unclosed string after comment with position"

(** {1 Predicate Combination Errors} *)

let test_conflicting_predicates () =
  (* Conflicting predicates - should this be allowed? *)
  assert_meta_parses
    ~content:"archive(byte,native) = \"impossible.cma\""
    ~name:"conflicting predicates allowed"

let test_duplicate_predicates () =
  assert_meta_parses
    ~content:"archive(byte,byte) = \"foo.cma\""
    ~name:"duplicate predicates allowed"

let test_negated_and_positive () =
  assert_meta_parses
    ~content:"requires(mt,-mt) = \"impossible\""
    ~name:"negated and positive predicate"

(** {1 Real-World Error Cases} *)

let test_missing_quotes_on_deps () =
  assert_meta_parse_error
    ~content:"requires = unix,str"
    ~expected_msg:"parsing requires failed"
    ~name:"requires without quotes"

let test_malformed_directory () =
  (* Directory field with special syntax *)
  assert_meta_parses
    ~content:"directory = \"^\""
    ~name:"caret directory syntax"

let test_plugin_without_predicate () =
  assert_meta_parses
    ~content:"plugin = \"foo.cma\""
    ~name:"plugin field without predicate"

(** {1 Test Suite} *)

let all_tests = [
  (* Predicate errors *)
  make_test "unclosed_predicate_paren" test_unclosed_predicate_paren;
  make_test "missing_predicate_close" test_missing_predicate_close;
  make_test "empty_predicate_parens" test_empty_predicate_parens;
  make_test "predicate_without_field" test_predicate_without_field;
  make_test "malformed_predicate" test_malformed_predicate;

  (* Field syntax errors *)
  make_test "missing_equals" test_missing_equals;
  make_test "missing_value" test_missing_value;
  make_test "field_without_value" test_field_without_value;
  make_test "duplicate_equals" test_duplicate_equals;
  make_test "invalid_field_chars" test_invalid_field_chars;

  (* String errors *)
  make_test "unclosed_string" test_unclosed_string;
  make_test "string_without_quotes" test_string_without_quotes;
  make_test "empty_string" test_empty_string;

  (* Package errors *)
  make_test "unclosed_package_block" test_unclosed_package_block;
  make_test "package_without_name" test_package_without_name;
  make_test "package_without_paren" test_package_without_paren;
  make_test "nested_package_error" test_nested_package_error;

  (* Append operator *)
  make_test "append_without_value" test_append_without_value;
  make_test "append_to_nonexistent" test_append_to_nonexistent;

  (* Specific fields *)
  make_test "requires_invalid_syntax" test_requires_invalid_syntax;
  make_test "archive_missing_predicate_close" test_archive_missing_predicate_close;
  make_test "ppx_invalid_syntax" test_ppx_invalid_syntax;
  make_test "linkopts_invalid" test_linkopts_invalid;

  (* Complex cases *)
  make_test "multiple_errors" test_multiple_errors;
  make_test "error_after_valid_content" test_error_after_valid_content;
  make_test "invalid_escape_sequence" test_invalid_escape_sequence;

  (* Lexer errors *)
  make_test "location_tracking" test_location_tracking;
  make_test "invalid_character" test_invalid_character;
  make_test "unexpected_eof" test_unexpected_eof;

  (* Comments *)
  make_test "comment_in_string" test_comment_in_string;
  make_test "unclosed_after_comment" test_unclosed_after_comment;

  (* Predicate combinations *)
  make_test "conflicting_predicates" test_conflicting_predicates;
  make_test "duplicate_predicates" test_duplicate_predicates;
  make_test "negated_and_positive" test_negated_and_positive;

  (* Real-world cases *)
  make_test "missing_quotes_on_deps" test_missing_quotes_on_deps;
  make_test "malformed_directory" test_malformed_directory;
  make_test "plugin_without_predicate" test_plugin_without_predicate;
]

let () = run_tests all_tests
