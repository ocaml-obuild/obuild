open Fugue
open Filepath
open Types
open Helper
open Printf
open Analyze
open Target
open Prepare
open Gconf
open Buildprogs

exception CCompilationFailed of string
exception CompilationFailed of string
exception Internal_Inconsistancy of string * string

(* check that destination is valid (mtime wise) against a list of srcs and
 * if not valid gives the filepath that has changed.
 * *)
let check_destination_valid_with srcs (_, dest) =
  if Filesystem.exists dest
  then (
    let dest_time = Filesystem.get_modification_time dest in
    try Some (List.find (fun (_,path) ->
        let mtime = Filesystem.get_modification_time path in
        dest_time < mtime
      ) srcs)
    with Not_found -> None
  ) else
    Some (Filetype.FileO, current_dir)

(* same as before but the list of sources is automatically determined
 * from the file DAG
*)
let check_destination_valid cstate (filety, dest) =
  let children =
    try Dag.get_children cstate.compilation_filesdag (Filetype.make_id (filety, dest))
    with Dag.DagNode_Not_found ->
      raise (Internal_Inconsistancy ((Filetype.to_string filety), ("missing destination: " ^ fp_to_string dest)))
  in
  check_destination_valid_with (List.map Filetype.get_id children) (filety,dest)

(* get a nice reason of why a destination is not deemed valid against
 * the source filepath that triggered the unvalid check.
 *
 * if source filepath is empty, it means that destination doesn't exists *)
let reason_from_paths (_,dest) (srcTy,changedSrc) =
  let trim_pd_exts z =
    let n = fn_to_string z in
    if string_endswith ".d" n then fn (Filename.chop_suffix n ".d")
    else if string_endswith ".p" n then fn (Filename.chop_suffix n ".p")
    else z
  in
  if changedSrc = current_dir
  then ""
  else (
    let bdest = path_basename dest in
    let bsrc  = path_basename changedSrc  in
    match Filetype.of_filename bdest with
    | Filetype.FileCMX | Filetype.FileCMO -> (
        match srcTy with
        | Filetype.FileCMX | Filetype.FileCMO ->
          let bml = Filetype.replace_extension bdest Filetype.FileML in
          let bmli = Filetype.replace_extension bdest Filetype.FileMLI in
          if bml = bsrc then "Source changed"
          else if bmli = bsrc then "Interface changed"
          else ("Dependency " ^ Modname.to_string (Modname.of_filename (trim_pd_exts bsrc)) ^ " changed " ^ fp_to_string changedSrc)
        | Filetype.FileCMXA | Filetype.FileCMA ->
          "Library changed " ^ fp_to_string changedSrc
        | _ ->
          "Dependencies changed " ^ fp_to_string changedSrc
      )
    | Filetype.FileO ->
      let bc = Filetype.replace_extension bdest Filetype.FileC in
      let bh = Filetype.replace_extension bdest Filetype.FileH in
      if bc = bsrc then ("C file " ^ fn_to_string bsrc ^ " changed")
      else if bh = bsrc then ("H file " ^ fn_to_string bsrc ^ " changed")
      else ("file changed " ^ fp_to_string changedSrc)
    | _ ->
      fp_to_string changedSrc ^ " changed"
  )

let get_all_modes target =
  let compile_opts = Target.get_compilation_opts target in
  let compiled_types = Target.get_ocaml_compiled_types target in
  let all_modes = List.concat (List.map (fun ty ->
      List.map (fun cmode -> (ty, cmode)) compile_opts) compiled_types) in
  List.filter (fun (t,o) ->
      match (t,o) with (ByteCode,WithProf) -> false | _ -> true) all_modes

let annot_mode () =
  if (Gconf.get_target_option "annot") && gconf.bin_annot then AnnotationBoth
  else if (Gconf.get_target_option "annot") then AnnotationText
  else if gconf.bin_annot then AnnotationBin
  else AnnotationNone

let get_nb_step dag =
  let nb_step = Dag.length dag in
  let nb_step_len = String.length (string_of_int nb_step) in
  (nb_step, nb_step_len)

let buildmode_to_filety bmode = if bmode = Native then Filetype.FileCMX else Filetype.FileCMO
let buildmode_to_library_filety bmode = if bmode = Native then Filetype.FileCMXA else Filetype.FileCMA

let internal_libs_paths self_deps =
  List.map (fun (compile_opt,compile_type) ->
      ((compile_opt,compile_type), List.map (fun dep ->
           let dirname = Dist.get_build_exn (Dist.Target (Name.Lib dep)) in
           let filety = buildmode_to_library_filety compile_type in
           let libpath = dirname </> Libname.to_cmca compile_type compile_opt dep in
           (filety, libpath)
         ) self_deps)
    ) [ (Normal,Native);(Normal,ByteCode);(WithProf,Native);(WithProf,ByteCode);(WithDebug,Native);(WithDebug,ByteCode)]

(* Helper: get include paths for ctypes from dependencies *)
let get_ctypes_includes bstate =
  let stdlib = fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config) in
  (* Find ctypes package paths *)
  try
    (* Get integers library path *)
    let (integers_path, integers_pkg) = Metacache.get "integers" in
    let integers_dir = Meta.get_include_dir stdlib (integers_path, integers_pkg) in

    (* Get str library path *)
    let (str_path, str_pkg) = Metacache.get "str" in
    let str_dir = Meta.get_include_dir stdlib (str_path, str_pkg) in

    let (path, pkg) = Metacache.get "ctypes" in
    let ctypes_dir = Meta.get_include_dir stdlib (path, pkg) in
    let ctypes_stubs_lib = Libname.of_string "ctypes.stubs" in
    let (stubs_path, stubs_root_pkg) = Metacache.get "ctypes" in
    let stubs_pkg = Meta.Pkg.find ctypes_stubs_lib.Libname.subnames stubs_root_pkg in
    let stubs_dir = Meta.get_include_dir stdlib (stubs_path, stubs_pkg) in
    [integers_dir; str_dir; ctypes_dir; stubs_dir]
  with _ -> []

(* Helper: get ctypes library files for linking *)
let get_ctypes_libs bstate =
  let stdlib = fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config) in
  try
    (* Get integers library (required by ctypes) *)
    let (integers_path, integers_pkg) = Metacache.get "integers" in
    let integers_dir = Meta.get_include_dir stdlib (integers_path, integers_pkg) in
    let integers_cma = integers_dir </> fn "integers.cma" in

    (* Get str library (required by ctypes.stubs) *)
    let (str_path, str_pkg) = Metacache.get "str" in
    let str_dir = Meta.get_include_dir stdlib (str_path, str_pkg) in
    let str_cma = str_dir </> fn "str.cma" in

    (* Get ctypes library *)
    let (path, pkg) = Metacache.get "ctypes" in
    let ctypes_dir = Meta.get_include_dir stdlib (path, pkg) in
    let ctypes_cma = ctypes_dir </> fn "ctypes.cma" in

    (* Get ctypes.stubs library *)
    let ctypes_stubs_lib = Libname.of_string "ctypes.stubs" in
    let (stubs_path, stubs_root_pkg) = Metacache.get "ctypes" in
    let stubs_pkg = Meta.Pkg.find ctypes_stubs_lib.Libname.subnames stubs_root_pkg in
    let stubs_dir = Meta.get_include_dir stdlib (stubs_path, stubs_pkg) in
    let stubs_cma = stubs_dir </> fn "ctypes_stubs.cma" in

    [integers_cma; str_cma; ctypes_cma; stubs_cma]
  with _ -> []

(* Generate ctypes.cstubs type discovery - produces types_generated.ml *)
let generate_cstubs_types task_index task lib bstate task_context dag =
  let (cstate, target) = Hashtbl.find task_context task in
  let (nb_step, nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Generating cstubs types for %s\n%!" nb_step_len task_index nb_step
    (Libname.to_string lib);
  match target.target_cstubs with
  | None ->
    verbose Report "  No cstubs configuration found\n%!";
    Scheduler.FinishTask task
  | Some cstubs ->
    let autogen_dir = get_cstubs_autogen_dir lib in
    let generated_types_name = cstubs.cstubs_generated_types in
    (* Write generated files to autogen directory - no placeholders needed *)
    let target_file = autogen_dir </> fn (String.uncapitalize_ascii generated_types_name ^ ".ml") in

    (* Check if we have type description *)
    match cstubs.cstubs_type_description with
    | None ->
      (* No type description - generate empty types module *)
      let content = Printf.sprintf
{|(* Auto-generated type bindings for %s *)
(* No type description specified *)
|}
        (Libname.to_string lib)
      in
      Filesystem.write_file target_file content;
      verbose Report "  Generated %s (no types)\n%!" (fp_to_string target_file);
      Scheduler.FinishTask task
    | Some type_desc ->
      (* Get the bindings module name *)
      let bindings_hier = type_desc.Target.cstubs_functor in
      let bindings_parts = Hier.to_string bindings_hier in
      (* Split "Bindings.Types" into module "Bindings" and functor "Types" *)
      let parts = String.split_on_char '.' bindings_parts in
      let (bindings_module, types_functor) = match parts with
        | [m; f] -> (m, f)
        | [m] -> (m, "Types")
        | _ -> (List.hd parts, "Types")
      in

      (* Get paths *)
      let ctypes_includes = get_ctypes_includes bstate in
      let build_dir = cstate.compilation_builddir_ml Normal in
      let src_dirs = target.target_obits.target_srcdir in

      (* Generate discover.ml that uses Cstubs_structs.write_c to discover type layouts.
         This properly handles structs defined in the user's Types functor. *)
      let discover_ml = autogen_dir </> fn "discover.ml" in
      let headers_str = String.concat "; "
        (List.map (fun h -> Printf.sprintf {|"#include <%s>\n"|} h) cstubs.cstubs_headers) in
      let headers_list = if headers_str = "" then "[]" else Printf.sprintf "[%s]" headers_str in
      let discover_content = Printf.sprintf
{|(* Auto-generated type discovery for %s using Cstubs_structs *)
let () =
  let headers = String.concat "" %s in
  let fmt = Format.std_formatter in
  Format.fprintf fmt "#include <stddef.h>@.";
  Format.fprintf fmt "#include <stdint.h>@.";
  Format.fprintf fmt "%%s" headers;
  Cstubs_structs.write_c fmt (module %s.%s)
|}
        (Libname.to_string lib)
        headers_list
        bindings_module
        types_functor
      in
      Filesystem.write_file discover_ml discover_content;
      verbose Report "  Generated %s\n%!" (fp_to_string discover_ml);

      (* Compile discover.ml - needs ctypes.stubs and the bindings module *)
      let discover_exe = autogen_dir </> fn "discover.byte" in
      let ocamlc = Prog.get_ocamlc () in
      let ctypes_libs = get_ctypes_libs bstate in
      let bindings_cmo = build_dir </> fn (String.uncapitalize_ascii bindings_module ^ ".cmo") in
      let include_args = List.concat [
        Utils.to_include_path_options ctypes_includes;
        Utils.to_include_path_options [build_dir];
        Utils.to_include_path_options src_dirs;
        Utils.to_include_path_options [autogen_dir];
      ] in
      let lib_args = List.map fp_to_string ctypes_libs in
      (* Check if bindings.cmo exists *)
      let bindings_exists = Filesystem.exists bindings_cmo in
      (* Link with ctypes libs and the user's bindings module *)
      let compile_args = [ocamlc] @ include_args @ lib_args @
        (if bindings_exists then [fp_to_string bindings_cmo] else []) @
        ["-o"; fp_to_string discover_exe; fp_to_string discover_ml] in

      verbose Report "  Compiling type discovery program...\n%!";
      (match Process.run compile_args with
      | Process.Failure err ->
        verbose Report "  Warning: Failed to compile discover.ml: %s\n%!" err;
        verbose Report "  Falling back to static type sizes\n%!";
        (* Fallback: generate static content *)
        let content = Printf.sprintf
{|(* Auto-generated type bindings for %s *)
(* Generated statically - type discovery compilation failed *)

let size_t_size = %d
|}
          (Libname.to_string lib)
          (Sys.word_size / 8)
        in
        Filesystem.write_file target_file content;
        Scheduler.FinishTask task
      | Process.Success _ ->
        (* Run discover to generate C code *)
        verbose Report "  Running type discovery program...\n%!";
        (match Process.run [fp_to_string discover_exe] with
        | Process.Failure err ->
          verbose Report "  Warning: Failed to run discover: %s\n%!" err;
          let content = Printf.sprintf
{|(* Auto-generated type bindings for %s *)
let size_t_size = %d
|}
            (Libname.to_string lib)
            (Sys.word_size / 8)
          in
          Filesystem.write_file target_file content;
          Scheduler.FinishTask task
        | Process.Success (c_code, _, _) ->
          (* Write the C program *)
          let discover_c = autogen_dir </> fn "discover.c" in
          Filesystem.write_file discover_c c_code;
          verbose Report "  Generated %s\n%!" (fp_to_string discover_c);

          (* Compile the C program - include ctypes directory for ctypes_cstubs_internals.h *)
          let discover_c_exe = autogen_dir </> fn "discover_c" in
          let cc = Prog.get_cc () in
          let ocaml_include = Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config in
          (* Get ctypes include paths for ctypes_cstubs_internals.h *)
          let ctypes_c_includes = List.concat_map
            (fun p -> ["-I"; fp_to_string p])
            ctypes_includes in
          let compile_c_args = [cc; "-I"; ocaml_include] @ ctypes_c_includes @
            ["-o"; fp_to_string discover_c_exe; fp_to_string discover_c] in

          verbose Report "  Compiling C type discovery...\n%!";
          (match Process.run compile_c_args with
          | Process.Failure err ->
            verbose Report "  Warning: Failed to compile discover.c: %s\n%!" err;
            let content = Printf.sprintf
{|(* Auto-generated type bindings for %s *)
let size_t_size = %d
|}
              (Libname.to_string lib)
              (Sys.word_size / 8)
            in
            Filesystem.write_file target_file content;
            Scheduler.FinishTask task
          | Process.Success _ ->
            (* Run the C program to get types *)
            verbose Report "  Running C type discovery...\n%!";
            (match Process.run [fp_to_string discover_c_exe] with
            | Process.Failure err ->
              verbose Report "  Warning: Failed to run C discover: %s\n%!" err;
              let content = Printf.sprintf
{|(* Auto-generated type bindings for %s *)
let size_t_size = %d
|}
                (Libname.to_string lib)
                (Sys.word_size / 8)
              in
              Filesystem.write_file target_file content;
              Scheduler.FinishTask task
            | Process.Success (ml_code, _, _) ->
              Filesystem.write_file target_file ml_code;
              verbose Report "  Generated %s\n%!" (fp_to_string target_file);
              ignore (bindings_module, types_functor);
              Scheduler.FinishTask task
            )
          )
        )
      )

(* Generate ctypes.cstubs function stubs - produces C.ml and stubs.c *)
let generate_cstubs_functions task_index task lib bstate task_context dag =
  let (cstate, target) = Hashtbl.find task_context task in
  let (nb_step, nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Generating cstubs functions for %s\n%!" nb_step_len task_index nb_step
    (Libname.to_string lib);
  match target.target_cstubs with
  | None ->
    verbose Report "  No cstubs configuration found\n%!";
    Scheduler.FinishTask task
  | Some cstubs ->
    let autogen_dir = get_cstubs_autogen_dir lib in
    let entry_point_name = cstubs.cstubs_generated_entry_point in
    let c_lib_name = cstubs.cstubs_external_library_name in
    let generated_types = cstubs.cstubs_generated_types in

    match cstubs.cstubs_function_description with
    | None ->
      (* No function description - generate minimal entry point *)
      let entry_file = autogen_dir </> fn (String.uncapitalize_ascii entry_point_name ^ ".ml") in
      let entry_content = Printf.sprintf
{|(* Auto-generated entry point for %s *)
module Types = %s
module Functions = struct end
|}
        (Libname.to_string lib)
        generated_types
      in
      Filesystem.write_file entry_file entry_content;
      (* Generate empty C stubs *)
      let c_stubs_file = autogen_dir </> fn (c_lib_name ^ "_stubs.c") in
      Filesystem.write_file c_stubs_file
{|/* Auto-generated C stubs - no functions bound */
#include <caml/mlvalues.h>
|};
      verbose Report "  Generated %s (no functions)\n%!" (fp_to_string entry_file);
      Scheduler.FinishTask task
    | Some func_desc ->
      let bindings_hier = func_desc.Target.cstubs_functor in
      let bindings_parts = Hier.to_string bindings_hier in
      let parts = String.split_on_char '.' bindings_parts in
      let (bindings_module, functions_functor) = match parts with
        | [m; f] -> (m, f)
        | [m] -> (m, "Functions")
        | _ -> (List.hd parts, "Functions")
      in

      (* Get types functor name from type description *)
      let types_functor = match cstubs.cstubs_type_description with
        | Some type_desc ->
          let type_parts = String.split_on_char '.' (Hier.to_string type_desc.Target.cstubs_functor) in
          (match type_parts with
           | [_; f] -> f
           | [_] -> "Types"
           | _ -> "Types")
        | None -> "Types"
      in

      (* Get paths *)
      let ctypes_includes = get_ctypes_includes bstate in
      let ctypes_libs = get_ctypes_libs bstate in
      let build_dir = cstate.compilation_builddir_ml Normal in
      let src_dirs = target.target_obits.target_srcdir in
      (* Write all generated ML files to autogen directory - no placeholders needed *)
      let ml_output_dir = autogen_dir in
      ignore src_dirs;

      (* Path to compiled bindings module *)
      let bindings_cmo = build_dir </> fn (String.uncapitalize_ascii bindings_module ^ ".cmo") in

      (* Generate stubgen.ml *)
      let stubgen_ml = autogen_dir </> fn "stubgen.ml" in
      let prefix = c_lib_name in
      let entry_file_name = String.uncapitalize_ascii entry_point_name ^ ".ml" in
      let stubs_file_name = c_lib_name ^ "_stubs.c" in

      (* Generate a module name for the generated FOREIGN implementation *)
      let generated_foreign_name = c_lib_name ^ "_generated" in
      let generated_foreign_file = String.uncapitalize_ascii generated_foreign_name ^ ".ml" in

      (* Convert concurrency policy to Cstubs module value string *)
      let concurrency_str = match cstubs.cstubs_concurrency with
        | Target.Cstubs_sequential -> "Cstubs.sequential"
        | Target.Cstubs_unlocked -> "Cstubs.unlocked"
        | Target.Cstubs_lwt_jobs -> "Cstubs.lwt_jobs"
        | Target.Cstubs_lwt_preemptive -> "Cstubs.lwt_preemptive"
      in

      (* Convert errno policy to Cstubs module value string *)
      let errno_str = match cstubs.cstubs_errno with
        | Target.Cstubs_ignore_errno -> "Cstubs.ignore_errno"
        | Target.Cstubs_return_errno -> "Cstubs.return_errno"
      in

      let stubgen_content = Printf.sprintf
{|(* Auto-generated stub generator for %s *)
let prefix = "%s"
let autogen_dir = "%s"
let ml_output_dir = "%s"

let () =
  (* Generate C stubs to autogen directory *)
  let c_file = open_out (Filename.concat autogen_dir "%s") in
  let c_fmt = Format.formatter_of_out_channel c_file in
  Format.fprintf c_fmt "/* Auto-generated by ctypes.cstubs */\n";
  Format.fprintf c_fmt "#include <caml/mlvalues.h>\n";
  Format.fprintf c_fmt "#include <caml/memory.h>\n";
  Format.fprintf c_fmt "#include <caml/alloc.h>\n";
  Format.fprintf c_fmt "#include <caml/custom.h>\n";
  Format.fprintf c_fmt "#include <caml/callback.h>\n";
  Format.fprintf c_fmt "#include <caml/fail.h>\n";
  Format.fprintf c_fmt "#include <string.h>\n";
  Format.fprintf c_fmt "\n";
  Cstubs.write_c c_fmt ~concurrency:%s ~errno:%s ~prefix (module %s.%s);
  close_out c_file;

  (* Generate FOREIGN implementation module to source directory *)
  let foreign_file = open_out (Filename.concat ml_output_dir "%s") in
  let foreign_fmt = Format.formatter_of_out_channel foreign_file in
  Format.fprintf foreign_fmt "(* Auto-generated FOREIGN implementation for %s *)\n";
  Cstubs.write_ml foreign_fmt ~concurrency:%s ~errno:%s ~prefix (module %s.%s);
  close_out foreign_file;

  (* Generate entry point that applies user's functor to generated module *)
  let entry_file = open_out (Filename.concat ml_output_dir "%s") in
  let entry_fmt = Format.formatter_of_out_channel entry_file in
  Format.fprintf entry_fmt "(* Auto-generated entry point for %s *)\n";
  Format.fprintf entry_fmt "(* Apply user's Types functor to generated TYPE implementation for struct layouts *)\n";
  Format.fprintf entry_fmt "module Types = %s.%s(%s)\n\n";
  Format.fprintf entry_fmt "(* Apply user's Functions functor to generated FOREIGN implementation *)\n";
  Format.fprintf entry_fmt "module C_Functions = %s.%s(%s)\n";
  close_out entry_file;

  print_endline "Stub generation complete"
|}
        (Libname.to_string lib)
        prefix
        (fp_to_string autogen_dir)
        (fp_to_string ml_output_dir)
        stubs_file_name
        concurrency_str errno_str
        bindings_module functions_functor
        generated_foreign_file
        (Libname.to_string lib)
        concurrency_str errno_str
        bindings_module functions_functor
        entry_file_name
        (Libname.to_string lib)
        bindings_module types_functor generated_types
        bindings_module functions_functor
        (String.capitalize_ascii generated_foreign_name)
      in
      Filesystem.write_file stubgen_ml stubgen_content;
      verbose Report "  Generated %s\n%!" (fp_to_string stubgen_ml);

      (* Compile stubgen.ml *)
      let stubgen_exe = autogen_dir </> fn "stubgen.byte" in
      let ocamlc = Prog.get_ocamlc () in
      let include_args = List.concat [
        Utils.to_include_path_options ctypes_includes;
        Utils.to_include_path_options [build_dir];
        Utils.to_include_path_options [autogen_dir];
        Utils.to_include_path_options src_dirs;
      ] in
      let lib_args = List.map fp_to_string ctypes_libs in

      (* Check if bindings.cmo exists *)
      let bindings_exists = Filesystem.exists bindings_cmo in
      verbose Report "  Bindings module at %s: %s\n%!"
        (fp_to_string bindings_cmo) (if bindings_exists then "found" else "not found");

      let compile_args = [ocamlc] @ include_args @ lib_args @
        (if bindings_exists then [fp_to_string bindings_cmo] else []) @
        ["-o"; fp_to_string stubgen_exe; fp_to_string stubgen_ml] in

      verbose Report "  Compiling stub generator...\n%!";
      (match Process.run compile_args with
      | Process.Failure err ->
        verbose Report "  Warning: Failed to compile stubgen.ml: %s\n%!" err;
        verbose Report "  Falling back to placeholder stubs\n%!";
        (* Fallback: generate placeholder content *)
        let entry_file = autogen_dir </> fn (String.uncapitalize_ascii entry_point_name ^ ".ml") in
        let entry_content = Printf.sprintf
{|(* Auto-generated entry point for %s *)
(* Stub generation failed - placeholder *)

module Types = %s

module Functions = struct
  (* Function stubs would be here *)
end
|}
          (Libname.to_string lib)
          generated_types
        in
        Filesystem.write_file entry_file entry_content;
        let c_stubs_file = autogen_dir </> fn (c_lib_name ^ "_stubs.c") in
        Filesystem.write_file c_stubs_file
{|/* Auto-generated C stubs - placeholder */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
|};
        verbose Report "  Generated placeholder stubs\n%!";
        Scheduler.FinishTask task
      | Process.Success _ ->
        (* Run stubgen *)
        verbose Report "  Running stub generator...\n%!";
        (match Process.run [fp_to_string stubgen_exe] with
        | Process.Failure err ->
          verbose Report "  Warning: Failed to run stubgen: %s\n%!" err;
          (* Fallback *)
          let entry_file = autogen_dir </> fn (String.uncapitalize_ascii entry_point_name ^ ".ml") in
          let entry_content = Printf.sprintf
{|(* Auto-generated entry point for %s *)
module Types = %s
module Functions = struct end
|}
            (Libname.to_string lib)
            generated_types
          in
          Filesystem.write_file entry_file entry_content;
          let c_stubs_file = autogen_dir </> fn (c_lib_name ^ "_stubs.c") in
          Filesystem.write_file c_stubs_file
{|/* Auto-generated C stubs */
#include <caml/mlvalues.h>
|};
          Scheduler.FinishTask task
        | Process.Success (output, _, _) ->
          verbose Report "  %s\n%!" output;
          verbose Report "  Stubs generated successfully\n%!";
          Scheduler.FinishTask task
        )
      )

(* Compile generated ctypes.cstubs C code *)
let compile_cstubs_c task_index task lib bstate task_context dag =
  let (cstate, target) = Hashtbl.find task_context task in
  let (nb_step, nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Compiling cstubs C for %s\n%!" nb_step_len task_index nb_step
    (Libname.to_string lib);
  match target.target_cstubs with
  | None ->
    verbose Report "  No cstubs configuration found\n%!";
    Scheduler.FinishTask task
  | Some cstubs ->
    let autogen_dir = get_cstubs_autogen_dir lib in
    let c_lib_name = cstubs.cstubs_external_library_name in
    let c_stubs_file = fn (c_lib_name ^ "_stubs.c") in
    (* Add ctypes include paths for C headers like ctypes_cstubs_internals.h *)
    let ctypes_c_includes = get_ctypes_includes bstate in
    let c_dir_spec = {
      include_dirs = cstate.compilation_c_include_paths @ ctypes_c_includes;
      dst_dir      = autogen_dir;
      src_dir      = autogen_dir
    } in
    let dest = (Filetype.FileO, c_dir_spec.dst_dir </> o_from_cfile c_stubs_file) in
    verbose Report "  Compiling %s -> %s\n%!" (fn_to_string c_stubs_file) (fp_to_string (snd dest));
    (* Use the C compiler to compile the stubs *)
    Scheduler.AddProcess (task, run_c_compile bstate.bstate_config c_dir_spec [] c_stubs_file)

(* compile C files *)
let compile_c task_index task c_file bstate task_context dag =
  let (cstate,target) = Hashtbl.find task_context task in
  let cbits = target.target_cbits in
  let c_dir_spec = {
    include_dirs = cstate.compilation_c_include_paths;
    dst_dir      = cstate.compilation_builddir_c;
    src_dir      = cbits.target_cdir
  } in
  let dest = (Filetype.FileO, c_dir_spec.dst_dir </> o_from_cfile c_file) in
  (match check_destination_valid cstate dest with
   | None            -> Scheduler.FinishTask task
   | Some src_changed ->
     let reason = reason_from_paths dest src_changed in
     let (nb_step,nb_step_len) = get_nb_step dag in
     verbose Report "[%*d of %d] Compiling C %-30s%s\n%!" nb_step_len task_index nb_step (fn_to_string c_file)
       (if reason <> "" then "    ( " ^ reason ^ " )" else "");
     let cflags = cbits.target_cflags in
     Scheduler.AddProcess (task, run_c_compile bstate.bstate_config c_dir_spec cflags c_file)
  )

(* compile a set of modules in directory into a pack *)
let compile_directory task_index task (h : Hier.t) task_context dag =
  let (cstate,target) = Hashtbl.find task_context task in
  let pack_opt = Hier.parent h in
  (* get all the modules defined at level h+1 *)
  let modules_task = Taskdep.linearize cstate.compilation_dag Taskdep.FromParent [task] in
  let filter_modules t : Hier.t option = match t with
    | (CompileC _) | (LinkTarget _) | (CheckTarget _) -> None
    | (GenerateCstubsTypes _) | (GenerateCstubsFunctions _) | (CompileCstubsC _) -> None
    | (CompileDirectory m) | (CompileModule m) -> if Hier.lvl m = (Hier.lvl h + 1) then Some m else None
    | (CompileInterface m) ->
      if Hier.lvl m = (Hier.lvl h + 1) then begin
        let fe = Hier.get_file_entry_maybe m in
        match fe with
          None -> None
        | Some e -> match e with
          Hier.FileEntry (_, f) ->
            if (Filetype.of_filepath f) = Filetype.FileMLI then
              Some m
            else None
          | _ -> None
      end
      else None
  in
  let modules = List.rev $ list_filter_map filter_modules modules_task in
  let all_modes = get_all_modes target in
  let annot_mode = annot_mode () in
  (* directory never have interface (?) so we serialize the native/bytecode creation.
   * the mtime checking is sub-optimal. low hanging fruits warning *)
  let tasks_ops : (string * Scheduler.call) option list list =
    let (byte_list,native_list) = List.partition (fun (t,_) -> t = ByteCode) all_modes in
    (List.map (fun pair_list ->
         List.map (fun (build_mode, comp_opt) ->
             let path = cstate.compilation_builddir_ml comp_opt in
             let dest = (Filetype.FileCMI, Hier.get_dest_file path Filetype.FileCMI h) in
             let mdeps = List.map (fun m ->
                 (Filetype.FileCMI, Hier.get_dest_file path Filetype.FileCMI m)
               ) modules in
             let dir = cstate.compilation_builddir_ml comp_opt in
             let fcompile = (fun () -> run_ocaml_pack dir dir annot_mode build_mode pack_opt h modules) in
             match check_destination_valid_with mdeps dest with
             | None            -> None
             | Some src_changed -> Some (reason_from_paths dest src_changed, fcompile)
           ) pair_list
       ) [byte_list; native_list])
  in
  let (reason, ops) =
    (*[ [(r,f)] ]*)
    let l : (string * Scheduler.call) list list = List.map maybes_to_list tasks_ops in
    match List.filter (fun x -> x <> []) l with
    | []                -> ("", [])
    | [] :: _          -> assert false
    | ((r,x)::xs) :: ys -> (r, (x :: List.map snd xs) :: List.map (List.map snd) ys)
  in
  if ops <> [] then (
    let (nb_step,nb_step_len) = get_nb_step dag in
    verbose Report "[%*d of %d] Packing %-30s%s\n%!" nb_step_len task_index nb_step (Hier.to_string h) reason;
    Scheduler.AddTask (task, ops)
  ) else
    Scheduler.FinishTask task

(** Helper: Check if recompilation is needed and prepare compilation functions

    Examines source files and dependencies to determine if recompilation is required.
    Returns a pair of (compilation_reason option, list of compilation functions).
 *)
let check_compilation_needed is_intf dep_descs dir_spec use_thread annot_mode pack_opt use_pp oflags h cstate =
  let rec check invalid descs = match descs with
    | []                                  -> (None, [])
    | (dest,build_mode,comp_opt,srcs) :: xs ->
      let r_dir_spec = {
        dir_spec with
        dst_dir = cstate.compilation_builddir_ml comp_opt <//> Hier.to_dirpath h;
        include_dirs = cstate.compilation_include_paths comp_opt h
      } in
      let fcompile =
        (build_mode,(fun () -> run_ocaml_compile r_dir_spec use_thread annot_mode build_mode comp_opt
                        pack_opt use_pp oflags h)) in
      if invalid
      then (
        let (_, ys) = check invalid xs in
        (Some "", fcompile :: ys)
      ) else (
        match check_destination_valid_with srcs dest with
        | None            -> check false xs
        | Some src_changed ->
          let reason = reason_from_paths dest src_changed in
          let (_, ys) = check true xs in
          (Some reason, fcompile :: ys)
      )
  in
  check false dep_descs

(** Helper: Organize compilation functions based on build modes

    Groups compilation functions appropriately:
    - Interface compilations run in parallel
    - Modules with interfaces run in parallel
    - Modules without interfaces partition native/bytecode builds
 *)
let organize_compilation_functions is_intf check_fun_list hdesc =
  if is_intf || Module.file_has_interface hdesc
  then [List.map snd check_fun_list]
  else let (l1,l2) = List.partition (fun (x,_) -> x = Compiled Native) check_fun_list in
    List.filter (fun x -> List.length x > 0) [List.map snd l1; List.map snd l2]

let dep_descs is_intf hdesc bstate cstate target h =
  let self_deps = Analyze.get_internal_library_deps bstate.bstate_config target in
  let internal_libs_paths_all_modes = internal_libs_paths self_deps in
  let module_deps = hdesc.Module.File.dep_cwd_modules in
  let compile_opts = Target.get_compilation_opts target in
  let all_modes = get_all_modes target in
  if is_intf then (
    let intf_desc =
      match hdesc.Module.File.intf_desc with
      | None      -> failwith "assertion error, task interface and no module_intf"
      | Some intf -> intf
    in
    List.map (fun comp_opt ->
        let path = cstate.compilation_builddir_ml comp_opt in
        let dest = (Filetype.FileCMI, Hier.get_dest_file path Filetype.FileCMI h) in
        let src  = [ (Filetype.FileMLI, intf_desc.Module.Intf.path) ] in
        let m_deps = List.map (fun module_dep ->
            (Filetype.FileCMI, Hier.get_dest_file path Filetype.FileCMI module_dep)) module_deps in
        let internal_deps = List.assoc (comp_opt,ByteCode) internal_libs_paths_all_modes in
        (dest,Interface,comp_opt, src @ internal_deps @ m_deps)
      ) compile_opts
  ) else (
    List.map (fun (compiled_ty, comp_opt) ->
        let file_compile_ty = buildmode_to_filety compiled_ty in
        let ext = if compiled_ty = ByteCode then Filetype.FileCMO else Filetype.FileCMX in
        let path = cstate.compilation_builddir_ml comp_opt in
        let dest = (file_compile_ty, Hier.get_dest_file path ext h) in
        let src = (match hdesc.Module.File.intf_desc with
              None -> []
            | Some intf -> [Filetype.FileMLI,intf.Module.Intf.path]) @ [(Filetype.FileML, hdesc.Module.File.path)] in
        let own_cmi_dep = (match hdesc.Module.File.intf_desc with
              None -> []
            | Some _ ->
                (* Add dependency on the module's own .cmi file *)
                [(Filetype.FileCMI, Hier.get_dest_file path Filetype.FileCMI h)]) in
        let m_deps = own_cmi_dep @ (List.concat (List.map (fun module_dep ->
            (* In bytecode mode, .cmo files only depend on .cmi files of dependencies.
               In native mode, .cmx files depend on both .cmx (for inlining) and .cmi *)
            let compiled_file_dep = if compiled_ty = Native
              then [(file_compile_ty, Hier.get_dest_file path ext module_dep)]
              else [] in
            compiled_file_dep @ [(Filetype.FileCMI, Hier.get_dest_file path Filetype.FileCMI module_dep)]
          ) module_deps)) in
        let internal_deps = List.assoc (comp_opt,compiled_ty) internal_libs_paths_all_modes in
        (dest,Compiled compiled_ty,comp_opt,src @ internal_deps @ m_deps)
      ) all_modes
  )

(* add a OCaml module or interface compilation process *)
let compile_module task_index task is_intf h bstate task_context dag =
  let all = Hashtbl.find_all task_context task in
  let process_one_target cstate target =
    let pack_opt = Hier.parent h in
    let hdesc =
      let desc = Hashtbl.find cstate.compilation_modules h in
      match desc with
      | Module.DescFile z -> z
      | Module.DescDir _  ->
        failwith (sprintf "internal error compile module on directory (%s). steps dag internal error"
                    (Hier.to_string h))
    in
    let src_path = path_dirname hdesc.Module.File.path in
    let use_thread = hdesc.Module.File.use_threads in
    let dir_spec = {
      src_dir      = src_path;
      dst_dir      = current_dir;
      include_dirs = [current_dir]
    } in
    let dep_descs = dep_descs is_intf hdesc bstate cstate target h in
    let annot_mode = annot_mode () in
    let check_result = check_compilation_needed is_intf dep_descs dir_spec use_thread annot_mode
                         pack_opt hdesc.Module.File.use_pp hdesc.Module.File.oflags h cstate in
    (check_result, hdesc)
  in
  let all = List.map (fun (c,t) -> process_one_target c t) all in
  let ((compilation_reason, _), _) = List.hd all in
  match compilation_reason with
  | None        -> Scheduler.FinishTask task
  | Some reason -> (* if the module has an interface, we create one list, so everything can be run in parallel,
                      * otherwise we partition the build_mode functions in build_modes group. *)
    let all_fun_lists = List.fold_left  (fun l ((_,check), hdesc) ->
        let funlist = organize_compilation_functions is_intf check hdesc in
        l @ funlist) [] all in

    let verb = if is_intf then "Intfing" else "Compiling" in
    let (nb_step, nb_step_len) = get_nb_step dag in
    verbose Report "[%*d of %d] %s %-30s%s\n%!" nb_step_len task_index nb_step verb (Hier.to_string h)
      (if reason <> "" then "    ( " ^ reason ^ " )" else "");
    Scheduler.AddTask (task, all_fun_lists)

let wait_for_files cdep_files =
  List.for_all (fun f ->
      let test = Filesystem.exists f in
      if not test then
        verbose Debug "warning: (temporarily?) missing file %s" (fp_to_string f);
      test
    ) cdep_files

let link_c cstate clib_name =
  let lib_name = cstate.compilation_builddir_c </> fn clib_name in
  let cdep_files = List.map (fun x -> cstate.compilation_builddir_c </> o_from_cfile x) cstate.compilation_csources in
  (* Not sure why it is necessary ... gcc seems to return before the files are ready. *)
  while not (wait_for_files cdep_files) do
    ignore (Unix.select [] [] [] 0.02)  (* sleep 1/50 second *)
  done;
  if gconf.ocamlmklib then
    [[(fun () -> run_c_linking LinkingShared cdep_files lib_name)]]
  else (
    let so_file = cstate.compilation_builddir_c </> fn ("dll" ^ clib_name ^ ".so") in
    let a_file = cstate.compilation_builddir_c </> fn ("lib" ^ clib_name ^ ".a") in
    [[(fun () -> run_c_linking LinkingShared cdep_files so_file)];
     [(fun () -> run_ar a_file cdep_files)];
     [(fun () -> run_ranlib a_file)]]
  )

let satisfy_preds dep preds =
  let satisfy_all current_pkg =
    let res = List.fold_left (fun acc (req_preds,req_libs) ->
        List.fold_left (fun _in_acc lib ->
            if lib = dep then Meta.Pkg.satisfy req_preds preds else true
          ) acc req_libs
      ) true current_pkg.Meta.Pkg.requires in
    res
  in
  let rec dep_is_satisfied current_pkg =
    (satisfy_all current_pkg) && (List.for_all satisfy_all current_pkg.subs) in
  let (_,root_pkg) = Metacache.get dep.Libname.main_name in
  dep_is_satisfied root_pkg

(** Helper: Resolve build dependencies to actual library file paths *)
let resolve_build_dependencies bstate pkgDeps compiledType compileOpt useThreadLib is_lib_target =
  let systhread = Analyze.get_ocaml_config_key_global "systhread_supported" in
  if is_lib_target then []
  else List.flatten (List.map (fun dep ->
      match Hashtbl.find bstate.bstate_config.project_dep_data dep with
      | Internal -> [(in_current_dir (Libname.to_cmca compiledType compileOpt dep))]
      | System   ->
        let (path, rootPkg) = Metacache.get_from_cache dep in
        let libDir = Meta.get_include_dir_with_subpath
            (fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config))
            (path, rootPkg) dep.Libname.subnames in
        let pred = match compiledType with
          | Native    -> Meta.Predicate.Native
          | ByteCode  -> Meta.Predicate.Byte
        in
        let preds = match useThreadLib with
          | PosixThread -> [ pred; Meta.Predicate.Mt; Meta.Predicate.Mt_posix]
          | VMThread -> [ pred; Meta.Predicate.Mt; Meta.Predicate.Mt_vm]
          | DefaultThread ->
            (if systhread = "true" then Meta.Predicate.Mt_posix else Meta.Predicate.Mt_vm) :: [ pred; Meta.Predicate.Mt]
          | NoThreads -> [ pred ]
        in
        let preds = match compileOpt with
          | WithProf -> Meta.Predicate.Gprof :: preds
          | _ -> preds
        in
        if (satisfy_preds dep preds) then
          let archives = Meta.Pkg.get_archive_with_filter (path, rootPkg) dep preds in
          List.fold_left (fun acc (_,a) ->
              let files = string_split ' ' a in
              acc @ (List.map (fun f -> libDir </> fn f) files)
            ) [] archives
        else
          []
    ) pkgDeps)

(** Helper: Calculate destination path for linked output *)
let get_link_destination cstate target compiledType compileOpt plugin =
  match target.target_name with
  | Name.Lib libname ->
    if plugin then
      cstate.compilation_builddir_ml Normal </> Libname.to_cmxs compileOpt libname
    else
      cstate.compilation_builddir_ml Normal </> Libname.to_cmca compiledType compileOpt libname
  | _ ->
    let outputName = Utils.to_exe_name compileOpt compiledType (Target.get_target_dest_name target) in
    cstate.compilation_builddir_ml Normal </> outputName

(** Helper: Wait for C object files to be ready with fresh modification times.

    Filesystem buffering can cause stat() to return stale mtimes even after
    the C compiler has finished. Poll until mtimes are fresh rather than
    using an arbitrary fixed delay. This implements the Phase 4 bug fix. *)
let wait_for_c_objects c_obj_files destTime =
  if List.length c_obj_files > 0 then (
    (* First wait for files to exist *)
    while not (wait_for_files c_obj_files) do
      ignore (Unix.select [] [] [] 0.02)  (* sleep 1/50 second *)
    done;
    (* Then poll until all files have mtimes newer than destTime *)
    let max_wait_time = Unix.gettimeofday () +. 5.0 in  (* 5 second safety timeout *)
    let rec poll_fresh () =
      if Unix.gettimeofday () > max_wait_time then
        verbose Debug "Warning: timeout waiting for C object mtimes to update\n"
      else
        let all_fresh = List.for_all (fun obj_file ->
          try
            let obj_mtime = Filesystem.get_modification_time obj_file in
            obj_mtime > destTime
          with _ -> false
        ) c_obj_files in
        if not all_fresh then (
          Unix.sleepf 0.01;  (* 10ms between polls *)
          poll_fresh ()
        )
    in
    poll_fresh ()
  )

(** Helper: Check if relinking is needed by comparing modification times *)
let check_needs_relink cstate compiled c_obj_files dest compiledType compileOpt =
  let destTime = Filesystem.get_modification_time dest in
  let ext = if compiledType = ByteCode then Filetype.FileCMO else Filetype.FileCMX in
  let path = cstate.compilation_builddir_ml compileOpt in

  (* Wait for C objects to have fresh mtimes *)
  wait_for_c_objects c_obj_files destTime;

  (* Check OCaml module files *)
  try Some (List.find (fun p -> destTime < Filesystem.get_modification_time p)
              (List.map (fun m -> Hier.get_dest_file path ext m) compiled))
  with Not_found ->
    (* Also check C object files *)
    try Some (List.find (fun p -> destTime < Filesystem.get_modification_time p) c_obj_files)
    with Not_found -> None

(** Main linking function - orchestrates dependency resolution, freshness checking, and linking *)
let link_ task_index bstate cstate pkgDeps target dag compiled useThreadLib cclibs compiledType compileOpt plugin =
  let buildDeps = resolve_build_dependencies bstate pkgDeps compiledType compileOpt useThreadLib (is_lib target) in
  let dest = get_link_destination cstate target compiledType compileOpt plugin in

  let linking_paths_of compileOpt = match compileOpt with
    | Normal    -> cstate.compilation_linking_paths
    | WithDebug -> cstate.compilation_linking_paths_d
    | WithProf  -> cstate.compilation_linking_paths_p
  in

  let c_obj_files = List.map (fun csrc -> cstate.compilation_builddir_c </> o_from_cfile csrc)
      cstate.compilation_csources in

  let depsTime = check_needs_relink cstate compiled c_obj_files dest compiledType compileOpt in

  if depsTime <> None then (
    let (nb_step,nb_step_len) = get_nb_step dag in
    let systhread = Analyze.get_ocaml_config_key_global "systhread_supported" in
    let link_type = if plugin then LinkingPlugin else
      if is_lib target then LinkingLibrary else LinkingExecutable in
    verbose Report "[%*d of %d] Linking %s %s\n%!" nb_step_len task_index nb_step
      (if is_lib target then "library" else "executable") (fp_to_string dest);
    [(fun () -> run_ocaml_linking (linking_paths_of compileOpt) compiledType
         link_type compileOpt useThreadLib systhread cclibs buildDeps compiled dest)]
  ) else []

let link task_index task bstate task_context dag =
  let (cstate,target) = Hashtbl.find task_context task in
  let cbits = target.target_cbits in
  let compiled = get_compilation_order cstate in
  verbose Debug "  compilation order: %s\n" (Utils.showList "," Hier.to_string compiled);
  let selfDeps = Analyze.get_internal_library_deps bstate.bstate_config target in
  verbose Debug "  self deps: %s\n" (Utils.showList "," Libname.to_string selfDeps);
  let selfLibDirs = List.map (fun dep -> Dist.get_build_exn (Dist.Target (Name.Lib dep))) selfDeps in
  (* Check for cstubs - if present, add the cstubs library *)
  let cstubs_cclibs = match target.target_cstubs with
    | Some cstubs ->
      let c_lib_name = cstubs.cstubs_external_library_name in
      [c_lib_name ^ "_stubs"]
    | None -> []
  in
  let cstubs_lib_dirs = match target.target_cstubs with
    | Some cstubs ->
      let libname = match target.target_name with
        | Name.Lib lib -> lib
        | _ -> failwith "cstubs only supported for libraries"
      in
      [get_cstubs_autogen_dir libname]
    | None -> []
  in
  let internal_cclibs = if cstate.compilation_csources <> []
    then [Target.get_target_clibname target]
    else []
  in
  let cclibs = List.concat (List.map (fun (cpkg,_) ->
      List.map (fun x -> "-l" ^ x)
        (Analyze.get_c_pkg cpkg bstate.bstate_config).cpkg_conf_libs) cbits.target_cpkgs)
               @ List.map (fun x -> "-L" ^ fp_to_string x) selfLibDirs
               @ List.map (fun x -> "-L" ^ fp_to_string x) cstubs_lib_dirs
               @ List.map (fun x -> "-l" ^ x) (cbits.target_clibs @ internal_cclibs @ cstubs_cclibs)
  in
  let pkgDeps = Analyze.get_pkg_deps target bstate.bstate_config in
  verbose Verbose "package deps: [%s]\n" (Utils.showList "," Libname.to_string pkgDeps);
  let useThreadLib =
    if List.mem (Libname.of_string "threads") pkgDeps then DefaultThread
    else if List.mem (Libname.of_string "threads.posix") pkgDeps then PosixThread
    else if List.mem (Libname.of_string "threads.vm") pkgDeps then VMThread
    else NoThreads
  in
  (* Create C library from regular C sources *)
  let cfunlist = if cstate.compilation_csources <> [] then
      link_c cstate (Target.get_target_clibname target)
    else [] in
  (* Create cstubs C library if needed *)
  let cstubs_cfunlist = match target.target_cstubs, target.target_name with
    | Some cstubs, Name.Lib libname ->
      let autogen_dir = get_cstubs_autogen_dir libname in
      let c_lib_name = cstubs.cstubs_external_library_name in
      let c_stubs_file = fn (c_lib_name ^ "_stubs.c") in
      let obj_file = autogen_dir </> o_from_cfile c_stubs_file in
      let lib_file = autogen_dir </> fn ("lib" ^ c_lib_name ^ "_stubs.a") in
      (* ar and ranlib must run sequentially - each in its own list *)
      [[(fun () -> run_ar lib_file [obj_file])];
       [(fun () -> run_ranlib lib_file)]]
    | _ -> []
  in
  let all_modes = get_all_modes target in
  let funlist = List.fold_left (fun flist (compiledType,compileOpt) ->
      let normal = (link_ task_index bstate cstate pkgDeps target dag compiled useThreadLib cclibs
          compiledType compileOpt false) in
      let res = if (is_lib target) && compiledType = Native && (Gconf.get_target_option "library-plugin") then
          (link_ task_index bstate cstate pkgDeps target dag compiled useThreadLib cclibs
             compiledType compileOpt true) @ normal
        else normal in
      res @ flist
    ) [] all_modes in
  if funlist <> [] then
    Scheduler.AddTask (task, cfunlist @ cstubs_cfunlist @ [funlist])
  else
    Scheduler.FinishTask task

let get_destination_files target =
  let all_modes = get_all_modes target in
  match target.Target.target_name with
  | Name.Lib libname ->
    List.map (fun (typ,opt) -> Libname.to_cmca typ opt libname) all_modes
  | Name.Exe _ | Name.Test _ | Name.Bench _ | Name.Example _ ->
    List.map (fun (ty,opt) ->
        Utils.to_exe_name opt ty (Target.get_target_dest_name target)
      ) all_modes

let sanity_check build_dir target =
  let files = get_destination_files target in
  let allOK = List.for_all (fun f ->
      let test = Filesystem.exists (build_dir </> f) in
      if not test then
        verbose Debug "warning: missing file %s" (fp_to_string (build_dir </> f));
      test
    ) files in
  if not allOK
  then verbose Report "warning: some target file appears to be missing";
  ()

let check task_index task task_context dag =
  let (_,target) = Hashtbl.find task_context task in
  let buildDir = Dist.get_build_path (Dist.Target target.target_name) in
  let (nb_step,nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Checking %s\n%!" nb_step_len task_index nb_step (fp_to_string buildDir);
  sanity_check buildDir target;
  Scheduler.FinishTask task

(* compile will process the compilation DAG,
 * which will compile all C sources and OCaml modules.
*)
let compile (bstate: build_state) task_context dag =
  let taskdep = Helper.Timing.measure_time "Taskdep.init" (fun () -> Taskdep.init dag) in
  (* a compilation task has finished, terminate the process,
     * and process the result *)
  let schedule_finish (task, st) is_done =
    (match Process.terminate (task, st) with
     | Process.Success (_, warnings, _) ->
       (* TODO: store warnings for !isDone and print them if they are different when isDone *)
       if is_done then print_warnings warnings
     | Process.Failure er            -> match task with
       | CompileC _ -> raise (CCompilationFailed er)
       | _          -> raise (CompilationFailed er)
    );
    if is_done then
      Taskdep.mark_done taskdep task
  in

  let dispatch (task_index, task) =
    match task with
    | (CompileC m)         -> compile_c task_index task m bstate task_context dag
    | (CompileInterface m) -> compile_module task_index task true m bstate task_context dag
    | (CompileModule m)    -> compile_module task_index task false m bstate task_context dag
    | (CompileDirectory m) -> compile_directory task_index task m task_context dag
    | (GenerateCstubsTypes lib)     -> generate_cstubs_types task_index task lib bstate task_context dag
    | (GenerateCstubsFunctions lib) -> generate_cstubs_functions task_index task lib bstate task_context dag
    | (CompileCstubsC lib)          -> compile_cstubs_c task_index task lib bstate task_context dag
    | (LinkTarget _)       -> link task_index task bstate task_context dag
    | (CheckTarget _)      -> check task_index task task_context dag
  in

  let stat = Helper.Timing.measure_time "Scheduler.schedule" (fun () ->
    Scheduler.schedule gconf.parallel_jobs taskdep dispatch schedule_finish
  ) in
  verbose Verbose "schedule finished: #processes=%d max_concurrency=%d\n" stat.Scheduler.nb_processes
    stat.Scheduler.max_runqueue;
  ()

let build_exe bstate exe =
  let target = Project.Executable.to_target exe in
  let modules = [Hier.of_filename exe.Project.Executable.main] in
  let task_context = Hashtbl.create 64 in
  let build_dir = Dist.create_build (Dist.Target target.target_name) in
  let cstate = prepare_target bstate build_dir target modules in
  List.iter (fun n -> Hashtbl.add task_context n (cstate,target))
    (Dag.get_nodes cstate.compilation_dag);
  compile bstate task_context cstate.compilation_dag

let rec select_leaves children duplicate dag =
  let (good,bad) = List.partition (fun a -> not (List.mem a duplicate)) children in
  let new_ = ref [] in
  List.iter (fun a ->
      let parents = Dag.get_parents dag a in
      List.iter (fun p -> new_ := p :: !new_) parents
    ) bad;
  if List.length bad > 0 then
    select_leaves (!new_ @ good) duplicate dag
  else
    good

let build_dag bstate proj_file targets_dag =
  Helper.Timing.measure_time "build_dag (total)" (fun () ->
    let dag = Helper.Timing.measure_time "DAG initialization" (fun () -> Dag.init ()) in
    let task_context = Hashtbl.create 64 in
    let taskdep = Taskdep.init targets_dag in
    let targets_deps = Hashtbl.create 64 in
    let prepare_state target modules =
      let build_dir = Dist.create_build (Dist.Target target.target_name) in
      let cstate = Helper.Timing.measure_time "prepare_target" (fun () ->
        prepare_target bstate build_dir target modules
      ) in
      List.iter (fun n -> Hashtbl.add task_context n (cstate,target))
        (Dag.get_nodes cstate.compilation_dag);
      let duplicate = Helper.Timing.measure_time "DAG merge" (fun () ->
        Dag.merge dag cstate.compilation_dag
      ) in
      (cstate.compilation_dag, duplicate)
    in
    Helper.Timing.measure_time "target preparation loop" (fun () ->
      while not (Taskdep.is_complete taskdep) do
        (match Taskdep.get_next taskdep with
         | None -> failwith "no free task in targets"
         | Some (_,ntask) ->
           verbose Verbose "preparing target %s\n%!" (Name.to_string ntask);
           let (cur_dag,dups) = (match ntask with
            | Name.Exe name   ->
              let exe = Project.find_exe proj_file name in
              prepare_state (Project.Executable.to_target exe) [Hier.of_filename exe.Project.Executable.main]
            | Name.Lib name   ->
              let lib = Project.find_lib proj_file name in
              prepare_state (Project.Library.to_target lib) lib.Project.Library.modules
            | Name.Bench name ->
              let bench = Project.find_bench proj_file name in
              prepare_state (Project.Bench.to_target bench) [Hier.of_filename bench.Project.Bench.main]
            | Name.Test name  ->
              let test = Project.find_test proj_file name in
              prepare_state (Project.Test.to_target test) [Hier.of_filename test.Project.Test.main]
            | Name.Example name ->
              let example = Project.find_example proj_file name in
              prepare_state (Project.Example.to_target example) [Hier.of_filename example.Project.Example.main]
           ) in
           if (Hashtbl.mem targets_deps ntask) then begin
             let children = Dag.get_leaves cur_dag in
             let children = select_leaves children dups cur_dag in
             let roots = Hashtbl.find targets_deps ntask in
             List.iter (fun child ->
                 List.iter (fun root ->
                     Dag.add_edge child root dag
                 ) roots
             ) children
           end;
           let roots = Dag.get_roots cur_dag in (* should be LinkTarget *)
           List.iter (fun p -> Hashtbl.add targets_deps p roots) (Dag.get_parents targets_dag ntask);
           Taskdep.mark_done taskdep ntask
        )
      done
    );
    Helper.Timing.measure_time "compilation phase" (fun () ->
      compile bstate task_context dag
    )
  )
