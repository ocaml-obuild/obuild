open Ext.Fugue
open Types
open Ext.Filepath

exception EmptyLibName

(* represent a library in a form abc[.def.xyz] *)
type t = { 
  main_name : string; 
  subnames : string list 
} 

let of_string s =
  match string_split '.' s with
  | []    -> raise EmptyLibName
  | x::xs -> { main_name = x; subnames = xs }
             
let to_string lname = String.concat "." (lname.main_name :: lname.subnames)
let to_string_nodes lname = lname.main_name :: lname.subnames
                                                     
let append lname sub = { lname with subnames = lname.subnames @ [sub] }

let to_libstring lib = String.concat "_" (to_string_nodes lib)
let to_cmxs (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cmxs")
let to_cmxa (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cmxa")
let to_cma (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cma")
let to_cmca b = if b = Native then to_cmxa else to_cma

(* only used for stdlib stuff *)
(*
let of_cmca b file =
  let suffix = if b = Native then ".cmxa" else ".cma" in
  Filename.chop_suffix (fn_to_string file) suffix
*)

