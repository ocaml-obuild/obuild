open Fugue

type verbosity_t =
  | Silent
  | Report
  | Verbose
  | Debug
  | Trace

type t = {
  mutable verbosity : verbosity_t;
  mutable parallel_jobs : int;
  mutable dump_dot : bool;
  mutable color : bool;
  mutable bin_annot : bool;
  mutable bin_annot_occurrences : bool;
  mutable short_path : bool;
  mutable ocamlmklib : bool;
  mutable ocaml_extra_args : string list;
}

type target_option =
  | Executable_profiling
  | Executable_debugging
  | Executable_native
  | Executable_bytecode
  | Executable_as_obj
  | Library_profiling
  | Library_debugging
  | Library_native
  | Library_bytecode
  | Library_plugin
  | Build_benchs
  | Build_tests
  | Build_examples
  | Annot

let all_target_options = [
  Executable_profiling; Executable_debugging; Executable_native; Executable_bytecode;
  Executable_as_obj; Library_profiling; Library_debugging; Library_native;
  Library_bytecode; Library_plugin; Build_benchs; Build_tests; Build_examples; Annot;
]

let target_option_to_string = function
  | Executable_profiling -> "executable-profiling"
  | Executable_debugging -> "executable-debugging"
  | Executable_native -> "executable-native"
  | Executable_bytecode -> "executable-bytecode"
  | Executable_as_obj -> "executable-as-obj"
  | Library_profiling -> "library-profiling"
  | Library_debugging -> "library-debugging"
  | Library_native -> "library-native"
  | Library_bytecode -> "library-bytecode"
  | Library_plugin -> "library-plugin"
  | Build_benchs -> "build-benchs"
  | Build_tests -> "build-tests"
  | Build_examples -> "build-examples"
  | Annot -> "annot"

exception UnknownOption of string

let target_option_of_string = function
  | "executable-profiling" -> Executable_profiling
  | "executable-debugging" -> Executable_debugging
  | "executable-native" -> Executable_native
  | "executable-bytecode" -> Executable_bytecode
  | "executable-as-obj" -> Executable_as_obj
  | "library-profiling" -> Library_profiling
  | "library-debugging" -> Library_debugging
  | "library-native" -> Library_native
  | "library-bytecode" -> Library_bytecode
  | "library-plugin" -> Library_plugin
  | "build-benchs" -> Build_benchs
  | "build-tests" -> Build_tests
  | "build-examples" -> Build_examples
  | "annot" -> Annot
  | s -> raise (UnknownOption s)

let env_variables =
  [
    "ocamlopt";
    "ocamlc";
    "ocaml";
    "ocamldep";
    "ocamldoc";
    "ocamlmklib";
    "ocamlmktop";
    "cc";
    "ranlib";
    "ar";
    "ld";
    "pkg-config";
    "camlp4";
    "findlib-path";
  ]

let env_ =
  let h : (string, string option) Hashtbl.t = Hashtbl.create (List.length env_variables) in
  List.iter (fun v -> Hashtbl.add h v None) env_variables;
  h

let get_env field = try Hashtbl.find env_ field with Not_found -> raise (UnknownOption field)

let set_env field value =
  if not (Hashtbl.mem env_ field) then raise (UnknownOption field);
  Hashtbl.replace env_ field (Some value)

let target_options_ =
  let h = Hashtbl.create (List.length all_target_options) in
  List.iter (fun opt ->
    let default = match opt with
      | Executable_native | Library_native | Library_bytecode -> true
      | Library_plugin -> Sys.os_type = "Unix"
      | _ -> false
    in
    Hashtbl.add h opt default) all_target_options;
  h

let rec set_target_option_typed opt value =
  Hashtbl.replace target_options_ opt value;
  match (opt, value) with
  | Executable_profiling, true -> set_target_option_typed Library_profiling true
  | Executable_debugging, true -> set_target_option_typed Library_debugging true
  | Library_plugin, true -> set_target_option_typed Library_native true
  | Library_native, false -> set_target_option_typed Library_plugin false
  | _ -> ()

let set_target_options field value =
  set_target_option_typed (target_option_of_string field) value

let get_target_options_keys () =
  List.map target_option_to_string all_target_options

let get_target_options () =
  Hashtbl.fold (fun k v acc -> (target_option_to_string k, v) :: acc) target_options_ []

let get_target_option_typed opt =
  try Hashtbl.find target_options_ opt with Not_found -> false

let get_target_option field =
  get_target_option_typed (target_option_of_string field)

let defaults =
  {
    verbosity = Report;
    parallel_jobs = 2;
    dump_dot = false;
    color = false;
    bin_annot = true;
    bin_annot_occurrences = false;
    short_path = false;
    ocamlmklib = true;
    ocaml_extra_args = [];
  }

let gconf = defaults
