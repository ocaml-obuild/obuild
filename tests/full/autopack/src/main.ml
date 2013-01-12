open Printf

let () =
    printf "A.foo: %s\n" A.foo;
    printf "B.A.foo: %s\n" B.A.foo;
    printf "B.C.foo: %s\n" B.C.foo;
    ()
