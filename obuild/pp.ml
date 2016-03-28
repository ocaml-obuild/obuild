open Ext.Fugue

exception InvalidPreprocessor of string

(*
http://ocaml.org/tutorials/camlp4_3.10.html
*)
type package = string list

module Type = struct
  type t = CamlP4O | CamlP4R

  let of_string s = match String.lowercase s with
    | "p4o" | "camlp4o" -> CamlP4O
    | "p4r" | "camlp4r" -> CamlP4R
    | _                 -> raise (InvalidPreprocessor s)

  let to_string = function
    | CamlP4O -> "camlp4o"
    | CamlP4R -> "camlp4r"
end

type desc = {
  camlp4   : string;
  packages : package list
}

type t = desc option

let some s pkgs = Some { camlp4 = s; packages = pkgs }
let none = None

let append pp pkgs = match pp with
  | None   -> pp
  | Some d -> Some { d with packages = d.packages @ pkgs }

let to_params pp =
  maybe [] (fun desc ->
      let s = desc.camlp4 ^ " " ^ String.concat " " (List.concat (List.map (fun x -> x) desc.packages)) in
      ["-pp"; s ]
    ) pp
