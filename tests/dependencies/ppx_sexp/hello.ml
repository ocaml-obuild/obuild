let _ =
  let a = [%sexp (define a "hi there!")] in
  Printf.printf "done\n"
