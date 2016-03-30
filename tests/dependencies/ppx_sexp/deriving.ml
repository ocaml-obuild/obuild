
type test = S of string [@@deriving show]

let () =
  let t = S "string" in
  Printf.printf "%s\n" (show_test t)
