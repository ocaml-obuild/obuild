open Uri
open Printf

let () =
    let u = Uri.make ~scheme:"http" ~host:"foo!.com" () in
    printf "%s\n" (Uri.to_string u)
