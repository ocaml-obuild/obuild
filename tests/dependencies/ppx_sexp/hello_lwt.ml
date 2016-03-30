open Lwt

let test () =
  let%lwt x = return 3 in
  return (x + 1 = 4)

let _ =
  Printf.printf "Done\n"
    
