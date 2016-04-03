open Obuild

let err = ref 0

let assumeEq testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else 
    (Printf.printf "FAILED %s Expected %b Got %b\n" testname expected got; err := !err + 1)

let expr_to_string = function
  | None -> ""
  | Some expr -> Expr.to_string expr

let eval version = function
  | None -> true
  | Some expr -> Expr.eval version expr

let () =
  let version1 = "1.7" in
  let version2 = "1.7.2" in
  let version3 = "2.0.0.0" in
  let version4 = "1.12.1alpha" in
  let (name,expr_ge) = Expr.parse_builddep "uri (>=1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_ge);
  assumeEq ">= false" false (eval version1 expr_ge);
  assumeEq ">= true" true (eval version2 expr_ge);
  assumeEq ">= true" true (eval version3 expr_ge);
  assumeEq ">= true" true (eval version4 expr_ge);
  let (name,expr_lt) = Expr.parse_builddep "uri (<1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_ge);
  assumeEq "< true" true (eval version1 expr_lt);
  assumeEq "< false" false (eval version2 expr_lt);
  assumeEq "< false" false (eval version3 expr_lt);
  assumeEq "< false" false (eval version4 expr_lt);
  let (name,expr_ne) = Expr.parse_builddep "uri (!=1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_ne);
  assumeEq "!= true" true (eval version1 expr_ne);
  assumeEq "!= false" false (eval version2 expr_ne);
  assumeEq "!= true" true (eval version3 expr_ne);
  assumeEq "!= true" true (eval version4 expr_ne);
  let (name,expr_not_eq) = Expr.parse_builddep "uri !(=1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_not_eq);
  assumeEq "! = true" true (eval version1 expr_ne);
  assumeEq "! = false" false (eval version2 expr_ne);
  assumeEq "! = true" true (eval version3 expr_ne);
  assumeEq "! = true" true (eval version4 expr_ne);
  let (name,expr_comp) = Expr.parse_builddep "uri (<1.7.2) || (>=2.0)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_comp);
  assumeEq "< | >= = true" true (eval version1 expr_comp);
  assumeEq "< | >= = false" false (eval version2 expr_comp);
  assumeEq "< | >= = true" true (eval version3 expr_comp);
  assumeEq "< | >= = false" false (eval version4 expr_comp);
  let (name,expr_comp2) = Expr.parse_builddep "uri ((<1.7.2) || (>=2.0) || (=1.7.2))" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_comp2);
  assumeEq "< | >= = true" true (eval version1 expr_comp2);
  assumeEq "< | >= = true" true (eval version2 expr_comp2);
  assumeEq "< | >= = true" true (eval version3 expr_comp2);
  assumeEq "< | >= = false" false (eval version4 expr_comp2);

  if !err > 1 then 
    exit 1
  else 
    exit 0

