open Printf

(* Simple unit test framework *)

type test_result = 
  | Success
  | TestFailure of string

type test_case = {
  name: string;
  test_func: unit -> test_result;
}

let test_count = ref 0
let failed_count = ref 0
let failed_tests = ref []

let assert_equal ~expected ~actual ~name =
  if expected = actual then
    Success
  else
    TestFailure (sprintf "Expected: %s, Got: %s" expected actual)

let assert_true ~actual ~name =
  if actual then
    Success
  else  
    TestFailure "Expected true, got false"

let assert_false ~actual ~name =
  if actual then
    TestFailure "Expected false, got true"
  else
    Success

let assert_raises ~expected_exn ~test_func ~name =
  try
    let _ = test_func () in
    TestFailure (sprintf "Expected exception %s, but no exception was raised"
             (Printexc.to_string expected_exn))
  with
  | exn when exn = expected_exn -> Success
  | exn -> TestFailure (sprintf "Expected exception %s, got %s"
                    (Printexc.to_string expected_exn)
                    (Printexc.to_string exn))

let assert_string_contains ~haystack ~needle ~name =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    Success
  with Not_found ->
    TestFailure (sprintf "Expected string to contain '%s', but it didn't.\nActual: %s"
             needle haystack)

let assert_no_exception ~test_func ~name =
  try
    let _ = test_func () in
    Success
  with exn ->
    TestFailure (sprintf "Expected no exception, but got: %s" (Printexc.to_string exn))

let assert_exception_message ~test_func ~expected_substring ~name =
  try
    let _ = test_func () in
    TestFailure "Expected exception to be raised, but no exception was raised"
  with exn ->
    let msg = Printexc.to_string exn in
    try
      let _ = Str.search_forward (Str.regexp_string expected_substring) msg 0 in
      Success
    with Not_found ->
      TestFailure (sprintf "Expected exception message to contain '%s', but got: %s"
               expected_substring msg)

let run_test test_case =
  incr test_count;
  printf "Running test: %s... " test_case.name;
  flush stdout;
  try
    match test_case.test_func () with
    | Success -> 
        printf "PASS\n"
    | TestFailure msg ->
        printf "FAIL: %s\n" msg;
        incr failed_count;
        failed_tests := test_case.name :: !failed_tests
  with
  | exn ->
      printf "ERROR: %s\n" (Printexc.to_string exn);
      incr failed_count;
      failed_tests := test_case.name :: !failed_tests

let run_tests tests =
  printf "Running %d tests...\n\n" (List.length tests);
  List.iter run_test tests;
  printf "\n";
  if !failed_count = 0 then begin
    printf "All %d tests passed!\n" !test_count;
    exit 0
  end else begin
    printf "%d of %d tests failed:\n" !failed_count !test_count;
    List.iter (printf "  - %s\n") (List.rev !failed_tests);
    exit 1
  end

let make_test name test_func = { name; test_func }