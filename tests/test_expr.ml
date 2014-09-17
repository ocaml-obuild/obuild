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
  let (name,expr_ge) = Expr.parse_builddep "uri (>=1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_ge);
  assumeEq ">= false" false (eval version1 expr_ge);
  assumeEq ">= true" true (eval version2 expr_ge);
  assumeEq ">= true" true (eval version3 expr_ge);
  let (name,expr_lt) = Expr.parse_builddep "uri (<1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_ge);
  assumeEq "< true" true (eval version1 expr_lt);
  assumeEq "< false" false (eval version2 expr_lt);
  assumeEq "< false" false (eval version3 expr_lt);
  let (name,expr_ne) = Expr.parse_builddep "uri (!=1.7.2)" in
  Printf.printf "pkg %s constraint %s\n" name (expr_to_string expr_ne);
  assumeEq "!= true" true (eval version1 expr_ne);
  assumeEq "!= false" false (eval version2 expr_ne);
  assumeEq "!= true" true (eval version3 expr_ne);

  if !err > 1 then 
    exit 1
  else 
    exit 0

