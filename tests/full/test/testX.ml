let failed = ref false
let runTest name f =
    let v = f () in
    if not v then failed := true;
    Printf.printf "test %s: %b\n" name v

let () =
    runTest "foo works" (fun () -> X.foo 12 12 = 12 + 12);
    if !failed then exit 1 else exit 0

