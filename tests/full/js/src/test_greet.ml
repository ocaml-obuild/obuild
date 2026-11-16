let () =
  if Greet.greeting "test" <> "hello test!" then (
    prerr_endline "unexpected greeting";
    exit 1);
  print_endline "greet ok"
