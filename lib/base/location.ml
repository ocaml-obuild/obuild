(** Source location tracking *)

(** A position in a source file *)
type loc = {
  line: int;
  col: int;
}

let new_location l c = {line = l; col = c}

(** A value paired with its source location *)
type 'a located = {
  value: 'a;
  loc: loc;
}

(** Create a located value *)
let located value loc = { value; loc }

(** Create a dummy location *)
let dummy_loc = { line = 0; col = 0 }

(** Create a located value with dummy location *)
let no_loc value = { value; loc = dummy_loc }
