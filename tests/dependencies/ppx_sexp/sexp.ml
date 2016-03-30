open Sexplib.Std

type t = {
  i: int;
  f: float
} [@@deriving sexp]

let () =
  let x = { i = 2048 ; f = 3.1415 } in
  let s = sexp_of_t x in
  let y = t_of_sexp s in
  assert(x = y)
