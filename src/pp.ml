open Ext

exception InvalidPreprocessor of string

(*
http://ocaml.org/tutorials/camlp4_3.10.html
*)

type pp_type = CamlP4O | CamlP4R

let pp_type_of_string s =
    match String.lowercase s with
    | "p4o" | "camlp4o" -> CamlP4O
    | "p4r" | "camlp4r" -> CamlP4R
    | _                 -> raise (InvalidPreprocessor s)

let pp_type_to_string ppty =
    match ppty with
    | CamlP4O -> "camlp4o"
    | CamlP4R -> "camlp4r"

type pp = { _pp : string option }

let pp_some s = { _pp = Some s }
let pp_none   = { _pp = None }

let pp_to_params pp = maybe [] (fun s -> ["-pp"; s ]) pp._pp

