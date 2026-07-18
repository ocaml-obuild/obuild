(* greeter is a vendored, packed library that itself depends on the vendored
   textutil library; everything must resolve internally *)
let () =
  let s = Greeter.Hello.greet "world" in
  print_endline s;
  if s <> "hello WORLD" then begin
    print_endline "FAILED: wrong output from vendored libraries";
    exit 1
  end
