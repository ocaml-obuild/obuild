open Printf

external geti : unit -> int = "stub_geti"

let inc a = a + 1

let () =
    printf "%d\n" (inc (geti ()));
    ()
