external gtk_true : unit -> bool = "stub_gtk_true"

let () =
    Printf.printf "gtk_true(): %b\n" (gtk_true ())
