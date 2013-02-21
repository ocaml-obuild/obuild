open Ext

exception InvalidPreprocessor of string

(*
http://ocaml.org/tutorials/camlp4_3.10.html
*)
type pp_package =
    { pp_pkg_strs    : string list
    }

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

type pp_desc =
    { pp_camlp4   : string
    ; pp_packages : pp_package list
    }
type pp = { _pp : pp_desc option }

let pp_some s pkgs = { _pp = Some { pp_camlp4 = s; pp_packages = pkgs } }
let pp_none   = { _pp = None }

let pp_append pp pkgs =
    match pp._pp with
    | None   -> pp
    | Some d -> { _pp = Some { d with pp_packages = d.pp_packages @ pkgs } }

let pp_to_params pp =
    maybe [] (fun desc ->
       let s = desc.pp_camlp4 ^ " " ^ String.concat " " (List.concat (List.map (fun x -> x.pp_pkg_strs) desc.pp_packages)) in
       ["-pp"; s ]
    ) pp._pp
