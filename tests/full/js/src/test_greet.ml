let () =
  if Greet.greeting "test" <> "hello test!" then (
    prerr_endline "unexpected greeting";
    exit 1);
  (* the test declares "rundir: rundir" *)
  if Filename.basename (Sys.getcwd ()) <> "rundir" then (
    prerr_endline ("unexpected working directory " ^ Sys.getcwd ());
    exit 1);
  print_endline "greet ok"
