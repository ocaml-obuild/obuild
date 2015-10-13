open Ext.Fugue

type verbosity_t = Silent | Report | Verbose | Debug | DebugPlus

type t = {
  mutable verbosity : verbosity_t;
  mutable strict    : bool;
  mutable parallel_jobs : int;
  mutable dump_dot : bool;
  mutable color : bool;
  mutable bin_annot : bool;
  mutable short_path : bool;
  mutable ocamlmklib : bool;
  mutable ocaml_extra_args : string list;
}

exception UnknownOption of string

let env_variables = [
  "ocamlopt"; "ocamlc"; "ocaml"; "ocamldep"; "ocamldoc"; "ocamlyacc"; "ocamllex"; "ocamlmklib";
  "ocamlmktop"; "cc"; "ranlib"; "ar"; "ld"; "pkg-config"; "camlp4"; "findlib-path"; "atdgen"
]

let env_ =
  let h : (string,string option) Hashtbl.t = Hashtbl.create (List.length env_variables) in
  List.iter (fun v -> Hashtbl.add h v None) env_variables;
  h

let get_env field = try
    Hashtbl.find env_ field
  with Not_found -> raise (UnknownOption field)

let set_env field value =
  if not (Hashtbl.mem env_ field) then raise (UnknownOption field);
  Hashtbl.replace env_ field (Some value)

let target_options_defaults = [
  ("executable-profiling", false);
  ("executable-debugging", false);
  ("executable-native", true);
  ("executable-bytecode", false);
  ("executable-as-obj", false);
  ("library-profiling", false);
  ("library-debugging", false);
  ("library-native", true);
  ("library-bytecode", true);
  ("library-plugin", (if Sys.os_type = "Unix" then true else false));
  ("build-benchs", false);
  ("build-tests", false);
  ("build-examples", false);
  ("annot", false);
]

let target_options_ =
  let h = Hashtbl.create (List.length target_options_defaults) in
  List.iter (fun (k,v) -> Hashtbl.add h k v) target_options_defaults;
  h

let rec set_target_options field value =
  if not (Hashtbl.mem target_options_ field) then raise (UnknownOption field);
  Hashtbl.replace target_options_ field value;
  (match field,value with
  | "executable-profiling", true -> set_target_options "library-profiling" true
  | "executable-debugging", true -> set_target_options "library-debugging" true
  | "library-plugin", true -> set_target_options "library-native" true
  | _ -> ())

let get_target_options_keys () = hashtbl_keys target_options_
let get_target_options () = hashtbl_toList target_options_
let get_target_option field = try
    Hashtbl.find target_options_ field
  with Not_found -> raise (UnknownOption field)

let defaults = {
  verbosity     = Report;
  strict        = false;
  parallel_jobs = 2;
  dump_dot      = false;
  color         = false;
  bin_annot     = true;
  short_path    = false;
  ocamlmklib    = true;
  ocaml_extra_args = [];
}

let gconf = defaults
