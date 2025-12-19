open Test_framework
open Test_helpers

(** Demo tests showing the new test framework capabilities *)

let test_string_contains_success () =
  assert_string_contains
    ~haystack:"Hello, World!"
    ~needle:"World"
    ~name:"string contains"

let test_string_contains_failure () =
  let result = assert_string_contains
    ~haystack:"Hello, World!"
    ~needle:"Missing"
    ~name:"string missing" in
  match result with
  | Failure _ -> Success  (* We expect this to fail *)
  | Success -> Failure "Should have failed"

let test_no_exception_success () =
  assert_no_exception
    ~test_func:(fun () -> 1 + 1)
    ~name:"simple math"

let test_exception_message () =
  assert_exception_message
    ~test_func:(fun () -> failwith "custom error message")
    ~expected_substring:"custom error"
    ~name:"exception contains message"

let test_meta_parse_minimal () =
  assert_meta_parses
    ~content:minimal_meta
    ~name:"minimal meta parses"

let test_meta_parse_field () =
  assert_meta_field
    ~content:minimal_meta
    ~pkg_name:"test"
    ~field_name:"version"
    ~expected_value:"1.0.0"
    ~test_name:"meta version field"

let test_meta_parse_error_demo () =
  assert_meta_parse_error
    ~content:"archive(byte \"missing equals\""
    ~expected_msg:"expecting ')'"
    ~name:"meta parse error"

let test_expr_parse_demo () =
  assert_expr_parses
    ~content:"(>= 1.0)"
    ~name:"expr parses"

let test_libname_parse_demo () =
  assert_libname_parse
    ~input:"foo.bar.baz"
    ~expected_main:"foo"
    ~expected_subs:["bar"; "baz"]
    ~name:"libname parse"

let all_tests = [
  make_test "string_contains_success" test_string_contains_success;
  make_test "string_contains_failure" test_string_contains_failure;
  make_test "no_exception_success" test_no_exception_success;
  make_test "exception_message" test_exception_message;
  make_test "meta_parse_minimal" test_meta_parse_minimal;
  make_test "meta_parse_field" test_meta_parse_field;
  make_test "meta_parse_error_demo" test_meta_parse_error_demo;
  make_test "expr_parse_demo" test_expr_parse_demo;
  make_test "libname_parse_demo" test_libname_parse_demo;
]

let () = run_tests all_tests
