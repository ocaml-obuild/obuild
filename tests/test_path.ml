open Obuild
open Ext

let err = ref 0

let assumeEq testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else 
    (Printf.printf "FAILED %s Expected %s Got %s\n" testname expected got; err := !err + 1)

let () =
  let b = Filepath.fp "src/b" in
  let b_abc = Hier.of_string "B.Abc" in
  let b_b_abc = Hier.add_prefix b b_abc in

  assumeEq "src/b + B.Abc" "src/b" (Filepath.fp_to_string b_b_abc);
(* Add_prefix src/b/abc B.Abc.Foo *)
  let b_abc = Filepath.fp "src/b/abc" in
  let b_abc_foo = Hier.of_string "B.Abc.Foo" in
  let b_abc_b_abc_foo = Hier.add_prefix b_abc b_abc_foo in

  assumeEq "src/b/abc + B.Abc.Foo" "src/b/abc" (Filepath.fp_to_string b_abc_b_abc_foo);
  if !err > 0 then 
    exit 1
  else 
    exit 0
