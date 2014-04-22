open Ext.Fugue

type verbosity = Silent | Report | Verbose | Debug | DebugPlus

type gconf = {
  mutable conf_verbosity : verbosity;
  mutable conf_withopt   : bool;
  mutable conf_strict    : bool;
  mutable conf_prog_ocamlopt  : string option;
  mutable conf_prog_ocamlc    : string option;
  mutable conf_prog_ocaml    : string option;
  mutable conf_prog_ocamldep  : string option;
  mutable conf_prog_ocamldoc  : string option;
  mutable conf_prog_ocamlyacc : string option;
  mutable conf_prog_ocamllex  : string option;
  mutable conf_prog_ocamlmklib : string option;
  mutable conf_prog_ocamlmktop : string option;
  mutable conf_prog_cc     : string option;
  mutable conf_prog_ranlib : string option;
  mutable conf_prog_ar     : string option;
  mutable conf_prog_ld     : string option;
  mutable conf_prog_pkgconfig : string option;
  mutable conf_prog_camlp4 : string option;
  mutable conf_findlib_path : string option;
  mutable conf_parallel_jobs : int;
  mutable conf_dump_dot : bool;
  mutable conf_color : bool;
  mutable conf_user_flags : (string * bool) list; (* TODO moved not as global variable *)
  mutable conf_bin_annot : bool;
  mutable conf_short_path : bool;
  mutable conf_ocamlmklib : bool;
}

exception UnknownOption of string

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
let get_target_option field =
  if not (Hashtbl.mem target_options_ field) then raise (UnknownOption field);
  Hashtbl.find target_options_ field

let gconf_defaults = {
  conf_verbosity     = Report;
  conf_withopt       = true;
  conf_strict        = false;
  conf_prog_ocamlopt = None;
  conf_prog_ocaml    = None;
  conf_prog_ocamlc   = None;
  conf_prog_ocamldep = None;
  conf_prog_ocamldoc = None;
  conf_prog_ocamlyacc = None;
  conf_prog_ocamllex = None;
  conf_prog_ocamlmklib = None;
  conf_prog_ocamlmktop = None;
  conf_prog_cc       = None;
  conf_prog_ranlib   = None;
  conf_prog_ar       = None;
  conf_prog_ld       = None;
  conf_prog_pkgconfig= None;
  conf_prog_camlp4   = None;
  conf_findlib_path  = None;
  conf_parallel_jobs        = 2;
  conf_dump_dot             = false;
  conf_color                = false;
  conf_user_flags           = [];
  conf_bin_annot            = true;
  conf_short_path           = false;
  conf_ocamlmklib           = true;
}

let gconf = gconf_defaults
