open Ext.Fugue

type verbosity = Silent | Report | Verbose | Debug | DebugPlus

type gconf = {
  mutable conf_verbosity : verbosity;
  mutable conf_strict    : bool;
  mutable conf_parallel_jobs : int;
  mutable conf_dump_dot : bool;
  mutable conf_color : bool;
  mutable conf_bin_annot : bool;
  mutable conf_short_path : bool;
  mutable conf_ocamlmklib : bool;
}

exception UnknownOption of string

let env_variables = [
  "ocamlopt"; "ocamlc"; "ocaml"; "ocamldep"; "ocamldoc"; "ocamlyacc"; "ocamllex"; "ocamlmklib";
  "ocamlmktop"; "cc"; "ranlib"; "ar"; "ld"; "pkg-config"; "camlp4"; "findlib-path"
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
  ("library-plugin", true);
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

let gconf_defaults = {
  conf_verbosity     = Report;
  conf_strict        = false;
  conf_parallel_jobs        = 2;
  conf_dump_dot             = false;
  conf_color                = false;
  conf_bin_annot            = true;
  conf_short_path           = false;
  conf_ocamlmklib           = true;
}

let gconf = gconf_defaults
