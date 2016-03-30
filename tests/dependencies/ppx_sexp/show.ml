type point2d = float * float
[@@deriving show]

let _ =
  Printf.printf "%s\n" (show_point2d (1.1,2.2));
  
