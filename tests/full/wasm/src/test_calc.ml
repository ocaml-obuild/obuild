let () =
  if Calc.sum [ 20; 22 ] <> 42 then exit 1;
  print_endline "calc ok"
