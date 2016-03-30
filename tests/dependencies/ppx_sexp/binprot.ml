open Bin_prot.Std

type t = {
  i: int;
  f: float
} [@@deriving bin_io]

let () =
  let x = { i = 2048 ; f = 3.1415 } in
  let buff = Bin_prot.Utils.bin_dump bin_writer_t x in
  let y = bin_read_t buff ~pos_ref:(ref 0) in
  assert(x = y)
