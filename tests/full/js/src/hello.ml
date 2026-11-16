external extra_answer : unit -> int = "extra_answer"

let () =
  print_endline (Greet.greeting "js");
  Printf.printf "answer=%d\n" (extra_answer ())
