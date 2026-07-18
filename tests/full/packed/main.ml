(* both libraries define a module Util; wrapping must keep them separate,
   and each library's own module must see its own Util *)
let () =
  let a = Liba.Alpha.who () in
  let b = Libb.Beta.who () in
  Printf.printf "alpha sees: %s\nbeta sees: %s\n" a b;
  if a <> "util-of-A" || b <> "util-of-B" then begin
    print_endline "FAILED: wrong Util linked";
    exit 1
  end
